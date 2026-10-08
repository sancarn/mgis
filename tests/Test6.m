let
    // Run as a blank query with the library query named mgis.
    // Explicit tree layouts make the pruning regressions independent of insertion order.
    createXY = mgis[gisLayerCreateFromTableWithXY],
    join = mgis[gisLayerJoinSpatial],
    operators = mgis[gisLayerQueryOperators],
    targets = createXY(
        #table({"ID", "X", "Y"}, {{"Far", -9, -1}, {"Near", 0.1, -1}, {"Parent", 0, 0}, {"Parent2", 0, 1}}),
        "X", "Y"
    ),
    rows = Table.ToRecords(targets[table]),
    // The index copies shapes with row IDs; the original table shapes retain null IDs.
    shapes = List.Transform(rows, (row) => Record.TransformFields(row[shape], {"__rowid__", each row[__rowid__]})),
    leaf = (envelope, shapes) => [envelope = envelope, capacity = 10, shapes = shapes, children = null],
    children = {
        leaf([MinX = -10, MinY = -10, MaxX = 0, MaxY = 0], {shapes{0}}),
        leaf([MinX = 0, MinY = -10, MaxX = 10, MaxY = 0], {shapes{1}}),
        leaf([MinX = 0, MinY = 0, MaxX = 10, MaxY = 10], {}),
        leaf([MinX = -10, MinY = 0, MaxX = 0, MaxY = 10], {})
    },
    fixture = (parents as list) => [
        table = Table.FirstN(targets[table], 2 + List.Count(parents)),
        geometryColumn = "shape",
        TProjection = null,
        queryLayer = [capacity = 10, root = [
            envelope = [MinX = -10, MinY = -10, MaxX = 10, MaxY = 10],
            capacity = 10, shapes = parents, children = children
        ]]
    ],
    adjacent = fixture({}),
    parent = fixture({shapes{2}}),
    twoParents = fixture({shapes{2}, shapes{3}}),
    sourceAt = (x, y) => createXY(#table({"X", "Y"}, {{x, y}}), "X", "Y"),
    nearestAt = (layer, x, y, operator) => join(sourceAt(x, y), layer, operator, "Left Outer")[table],
    acrossBoundary = nearestAt(adjacent, -0.1, -1, operators[gisNearest]),
    outsideRoot = nearestAt(adjacent, 20, 20, operators[gisNearest]),
    belowParent = nearestAt(parent, -9, -1, operators[gisNearest]),
    nearestTwo = nearestAt(twoParents, -9, -1, operators[gisNearestN](2)),
    moreThanAvailable = nearestAt(adjacent, -0.1, -1, operators[gisNearestN](5)),
    empty = mgis[gisLayerCreateBlank]("shape", 10),
    noTarget = nearestAt(empty, 20, 20, operators[gisNearest]),
    // The nearest distances are 9.99, exactly 10, and 10.01 units.
    cutoffTargets = createXY(#table({"ID", "X", "Y"}, {{"Boundary", 10, 0}}), "X", "Y"),
    cutoffSources = createXY(#table({"ID", "X", "Y"}, {{"Inside", 0.01, 0}, {"Boundary", 0, 0}, {"Outside", -0.01, 0}}), "X", "Y"),
    cutoffJoin = join(cutoffSources, cutoffTargets, operators[gisNearest], "Inner")[table],
    withinTen = Table.SelectRows(cutoffJoin, each [dist] <= 10),
    // Existing custom operators must retain overlap pruning.
    customIntersects = [
        onCandidate = operators[gisEnvelopeIntersects][onCandidate],
        combine = null, continueSearch = null, additionalColumns = null
    ],
    intersects = nearestAt(adjacent, -0.1, -1, customIntersects),
    exactIntersection = nearestAt(adjacent, -9, -1, operators[gisEnvelopeIntersects]),
    // Also exercise the normal layer builder instead of only controlled tree fixtures.
    builtLayer = nearestAt(targets, -0.1, -1, operators[gisNearest]),
    // Fail if the distant branch's candidate is evaluated: this proves pruning,
    // rather than merely checking that a full scan returns the correct result.
    pruningOperator = Record.Combine({operators[gisNearest], [
        onCandidate = (candidate, query) =>
            if candidate[__rowid__] = 1 then error "Distant branch should have been pruned."
            else operators[gisNearest][onCandidate](candidate, query)
    ]}),
    prunedResult = nearestAt(adjacent, -8.5, -1, pruningOperator),
    manyPoints = List.Transform({0..31}, (i) => {i, Number.Mod(i * 7, 97) - 50, Number.Mod(i * 11, 89) - 45}),
    manyTargets = createXY(#table({"ID", "X", "Y"}, manyPoints), "X", "Y"),
    indexShapes = (node as record) as list => node[shapes] & (
        if node[children] = null then {}
        else List.Combine(List.Transform(node[children], each @indexShapes(_)))
    ),
    indexed = indexShapes(manyTargets[queryLayer][root]),
    inserted = mgis[gisLayerInsertRows](manyTargets, {
        [ID = 32, X = -100, Y = -100, shape = mgis[gisShapeCreateFromWKT]("POINT(-100 -100)")]
    }),
    insertedNearest = nearestAt(inserted, -100, -100, operators[gisNearest]),
    allNodes = (node as record) as list => {node} & (
        if node[children] = null then {}
        else List.Combine(List.Transform(node[children], each @allNodes(_)))
    ),
    validCache = (layer) => List.AllTrue(List.Transform(allNodes(layer[queryLayer][root]), (node) =>
        let
            centres = List.Transform(indexShapes(node), (s) => {
                (s[Envelope][MinX] + s[Envelope][MaxX]) / 2,
                (s[Envelope][MinY] + s[Envelope][MaxY]) / 2
            }),
            expected = if List.IsEmpty(centres) then null else [
                MinX = List.Min(List.Transform(centres, each _{0})),
                MinY = List.Min(List.Transform(centres, each _{1})),
                MaxX = List.Max(List.Transform(centres, each _{0})),
                MaxY = List.Max(List.Transform(centres, each _{1}))
            ]
        in node[nearestEnvelope] = expected
    )),
    tiedTargets = createXY(#table({"ID", "X", "Y"}, {{"A", -1, -1}, {"B", 1, 1}}), "X", "Y"),
    tiedOne = nearestAt(tiedTargets, 0, 0, operators[gisNearest]),
    tiedTwo = nearestAt(tiedTargets, 0, 0, operators[gisNearestN](2)),
    zeroNeighbours = nearestAt(adjacent, 0, 0, operators[gisNearestN](0)),
    nonPointTargets = mgis[gisLayerCreateFromTableWithWKT](
        #table({"ID", "shape"}, {{"Line", "LINESTRING(-100 0,100 0)"}, {"Point", "POINT(1 0)"}}), "shape"
    ),
    nearestCentre = nearestAt(nonPointTargets, 0.25, 0, operators[gisNearest]),
    queries = {{-100, -100}, {-25, 13}, {0, 0}, {17, -8}, {100, 100}},
    bruteForceChecks = List.Combine(List.Transform(queries, (xy) =>
        List.Transform({1, 3, 40}, (k) =>
            let
                distances = List.Sort(List.Transform(manyPoints, (point) =>
                    Number.Sqrt(Number.Power(point{1} - xy{0}, 2) + Number.Power(point{2} - xy{1}, 2))
                )),
                expected = List.FirstN(distances, k),
                actual = List.Sort(Table.Column(nearestAt(manyTargets, xy{0}, xy{1}, operators[gisNearestN](k)), "dist"))
            in
                List.Count(actual) = List.Count(expected) and List.AllTrue(List.Transform(
                    List.Positions(expected), (i) => Number.Abs(actual{i} - expected{i}) < 0.000000001
                ))
        )
    )),
    checks = [
        NearestAcrossBranchBoundary = acrossBoundary{0}[layer2][ID] = "Near"
            and Number.Abs(acrossBoundary{0}[dist] - 0.2) < 0.000000001,
        NearestOutsideRoot = outsideRoot{0}[layer2][ID] = "Near"
            and Number.Abs(outsideRoot{0}[dist] - Number.Sqrt(19.9 * 19.9 + 21 * 21)) < 0.000000001,
        CloserChildThanParent = belowParent{0}[layer2][ID] = "Far" and belowParent{0}[dist] = 0,
        NearestNReplacesParentCandidates = List.Sort(List.Transform(Table.ToRecords(nearestTwo), each [layer2][ID])) = {"Far", "Parent"},
        NearestNReturnsAllWhenKExceedsCount = Table.RowCount(moreThanAvailable) = 2,
        EmptyTargetLeftOuter = Table.RowCount(noTarget) = 1 and noTarget{0}[layer2] = null and noTarget{0}[dist] = null,
        TenUnitCutoff = List.Transform(Table.ToRecords(withinTen), each [layer1][ID]) = {"Inside", "Boundary"},
        CustomOperatorStillPrunes = intersects{0}[layer2] = null,
        IntersectionStillMatches = exactIntersection{0}[layer2][ID] = "Far",
        BuiltLayerNearest = builtLayer{0}[layer2][ID] = "Near",
        BuiltTreeMatchesBruteForce = List.AllTrue(bruteForceChecks),
        DistantBranchIsPruned = prunedResult{0}[layer2][ID] = "Far" and prunedResult{0}[dist] = 0.5,
        SubdivisionDoesNotDuplicateShapes = List.Sort(List.Transform(indexed, each [__rowid__])) = {0..31},
        InsertUpdatesNearestBounds = insertedNearest{0}[layer2][ID] = 32 and insertedNearest{0}[dist] = 0,
        CachedBoundsMatchDescendantCentres = validCache(manyTargets) and validCache(inserted),
        EqualDistanceTies = Table.RowCount(tiedOne) = 1 and List.Contains({"A", "B"}, tiedOne{0}[layer2][ID])
            and Number.Abs(tiedOne{0}[dist] - Number.Sqrt(2)) < 0.000000001
            and List.Sort(Table.TransformRows(tiedTwo, each [layer2][ID])) = {"A", "B"},
        ZeroNeighbours = Table.RowCount(zeroNeighbours) = 1 and zeroNeighbours{0}[layer2] = null,
        NonPointUsesEnvelopeCentre = nearestCentre{0}[layer2][ID] = "Line" and nearestCentre{0}[dist] = 0.25
    ]
in
    if List.AllTrue(Record.FieldValues(checks)) then checks
    else error Error.Record("TestFailure", "Nearest-neighbour regression checks failed.", checks)
