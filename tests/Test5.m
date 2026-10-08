let
    // Run as a blank query with the library query named mgis. All checks should be true.
    createXY = mgis[gisLayerCreateFromTableWithXY],
    source = #table(
        type table [Name = text, #"X coordinate" = number, #"Y coordinate" = number],
        {{"A", -12.5, 48.25}, {"B", 0, -3.5}, {"C", 20, 10}}
    ),
    layer = createXY(source, "X coordinate", "Y coordinate"),
    rows = Table.ToRecords(layer[table]),
    expected = List.Transform(
        {"POINT(-12.5 48.25)", "POINT(0 -3.5)", "POINT(20 10)"},
        each mgis[gisShapeCreateFromWKT](_)
    ),
    emptyLayer = createXY(Table.FirstN(source, 0), "X coordinate", "Y coordinate"),
    missingX = try createXY(source, "Missing", "Y coordinate")[table],
    missingY = try createXY(source, "X coordinate", "Missing")[table],
    collision = try createXY(Table.AddColumn(source, "shape", each "existing"), "X coordinate", "Y coordinate")[table],
    indexShapes = (node as record) as list => node[shapes] & (
        if node[children] = null then {}
        else List.Combine(List.Transform(node[children], each @indexShapes(_)))
    ),
    indexed = indexShapes(layer[queryLayer][root]),
    checks = [
        PointGeometryAndEnvelopes = List.AllTrue(List.Transform(
            List.Positions(rows),
            (i) => rows{i}[shape][Kind] = "POINT"
                and rows{i}[shape][Geometry] = expected{i}[Geometry]
                and rows{i}[shape][Envelope] = expected{i}[Envelope]
        )),
        OriginalColumnsPreserved = Table.SelectColumns(layer[table], Table.ColumnNames(source)) = source,
        RowIds = Table.Column(layer[table], "__rowid__") = {0, 1, 2},
        GeometryColumn = layer[geometryColumn] = "shape",
        SpatialIndex = List.Sort(List.Transform(indexed, each _[__rowid__])) = {0, 1, 2}
            and List.AllTrue(List.Transform(indexed, each _[Geometry] = expected{_[__rowid__]}[Geometry])),
        EmptyTable = Table.RowCount(emptyLayer[table]) = 0
            and Table.HasColumns(emptyLayer[table], {"shape", "__rowid__"})
            and emptyLayer[queryLayer][root][shapes] = {},
        MissingXRejected = missingX[HasError],
        MissingYRejected = missingY[HasError],
        ShapeCollisionRejected = collision[HasError]
    ]
in
    if List.AllTrue(Record.FieldValues(checks)) then checks
    else error Error.Record("TestFailure", "XY layer regression checks failed.", checks)
