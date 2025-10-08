let
    //**********************
    //* 🗺️ GIS LIBRARY 🗺️ *
    //**********************
    //@url: https://github.com/sancarn/mgis
    //@description: GIS library for the PowerQuery M-lang ecosystem.
    //@author: Sancarn

    //Base types as outputted by Geometry.FromWellKnownText() base function.
    TBasePoint = type [
        Kind = Value.Type("POINT"),
        X = number,
        Y = number
    ],
    TBaseLineString = type [
        Kind = Value.Type("LINESTRING"),
        Points = {TBasePoint}
    ],
    TBasePolygon = type [
        Kind = Value.Type("POLYGON"),
        Rings = {TBaseLineString}
    ],
    TBaseMultiPoint = type [
        Kind = Value.Type("MULTIPOINT"),
        Components = {TBasePoint}
    ],
    TBaseMultiLineString = type [
        Kind = Value.Type("MULTILINESTRING"),
        Components = {TBaseLineString}
    ],
    TBaseMultiPolygon = type [
        Kind = Value.Type("MULTIPOLYGON"),
        Components = {TBasePolygon}
    ],
    TBaseGeometryCollection = type [
        Kind = Value.Type("GEOMETRYCOLLECTION"),
        Components = {any}                        //Hack: No union types in M-lang
    ],
    TShapeEnvelope = type [
        MinX = number,
        MinY = number,
        MaxX = number,
        MaxY = number
    ],
    TShape = type [
        //Kind as "POINT" | "LINESTRING" | "POLYGON" | "MULTIPOINT" | "MULTILINESTRING" | "MULTIPOLYGON" | "GEOMETRYCOLLECTION"
        Kind = text,
        //Geometry as TBasePoint | TBaseLineString | TBasePolygon | TBaseMultiPoint | TBaseMultiLineString | TBaseMultiPolygon | TBaseGeometryCollection
        Geometry = any,  //Hack: No union types in M-lang
        Envelope = TShapeEnvelope,
        __rowid__ = nullable number
    ],
    privGetEnvelope = (geometry as any) as record => (
        //Recursive switch statement based on geometry.Kind
        let 
            envelopsToEnvelope = (envelopes as list) as record => (
                let
                    MinXs = List.Transform(envelopes, each _[MinX]),
                    MinYs = List.Transform(envelopes, each _[MinY]),
                    MaxXs = List.Transform(envelopes, each _[MaxX]),
                    MaxYs = List.Transform(envelopes, each _[MaxY])
                in [
                    MinX = List.Min(MinXs),
                    MinY = List.Min(MinYs),
                    MaxX = List.Max(MaxXs),
                    MaxY = List.Max(MaxYs)
                ]
            ),
            switch = [
                POINT = (geometry as record) => [
                    MinX = geometry[X],
                    MinY = geometry[Y],
                    MaxX = geometry[X],
                    MaxY = geometry[Y]
                ],
                LINESTRING = (geometry as record) => (
                    let
                        //Compute envelopes for each point recursively
                        envelopes = List.Transform(geometry[Points], each @privGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                POLYGON = (geometry as record) => (
                    let
                        //Compute envelopes for each ring recursively
                        envelopes = List.Transform(geometry[Rings], each @privGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                MULTIPOINT = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @privGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                MULTILINESTRING = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @privGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)    
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                MULTIPOLYGON = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @privGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                GEOMETRYCOLLECTION = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @privGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                )
            ]
        in
            Record.Field(switch, geometry[Kind])(geometry)
    ),
    ShapeCreateFromWKT = Value.ReplaceType(
        (wkt as text) as record => (
            let 
                baseGeometry = Geometry.FromWellKnownText(wkt),
                envelope = privGetEnvelope(baseGeometry)
            in [
                Kind = baseGeometry[Kind],
                Geometry = baseGeometry,
                Envelope = envelope,
                __rowid__ = null
            ] 
        ), 
        type function (wkt as text) as TShape
    ),
    // Ensures a table has a numeric __rowid__ column unique per row. If absent, adds it as index starting 0.
    EnsureRowIdColumn = (tbl as table) as table => (
        let
            hasCol = Table.HasColumns(tbl, "__rowid__"),
            withId = if hasCol then tbl else Table.AddIndexColumn(tbl, "__rowid__", 0, 1, Int64.Type)
        in
            withId
    ),
    TQuadTreeNode = type [
        //Shapes is where the actual shapes are stored. Sometimes a polygon or line may overlap multiple leaves,
        //we store these in the parent node's children list.
        shapes = {TShape},
        
        //The capacity of a node before it is split into children.
        capacity = number,

        //Children is where we store any children nodes/leaves. All leaves are nodes (without `children`)
        children = nullable {@TQuadTreeNode},

        //The envelope of the node.
        envelope = TShapeEnvelope
    ],
    TQuadTree = type [
        //The top level quadtree node.
        root = TQuadTreeNode,
        
        //The capacity of a node before it is split into children.
        capacity = number
    ],
    TLayer = type [
        table = table,
        geometryColumn = text,
        queryLayer = TQuadTree
    ],
    TQuadTreeQueryOperator = type function (candidate as TShape, q as TShape) as logical,
    TRowQueryOperator = type function (candidate as record) as logical,
    QuadTreeBoxesIntersect = Value.ReplaceType(
        (b1 as record, b2 as record) as logical => (
            not (
                b1[MaxX] < b2[MinX] or
                b1[MinX] > b2[MaxX] or
                b1[MaxY] < b2[MinY] or
                b1[MinY] > b2[MaxY]
            )
        ),
        type function (b1 as TShapeEnvelope, b2 as TShapeEnvelope) as logical
    ),
    QuadTreeSubdivideNode = Value.ReplaceType(
        (node as record) as record => (
            let
                midX = (node[envelope][MinX] + node[envelope][MaxX]) / 2,
                midY = (node[envelope][MinY] + node[envelope][MaxY]) / 2,
                b = node[envelope],
                children = {
                    // SW
                    [envelope = [MinX = b[MinX], MinY = b[MinY], MaxX = midX, MaxY = midY], capacity = node[capacity], shapes = {}, children = null],
                    // SE
                    [envelope = [MinX = midX, MinY = b[MinY], MaxX = b[MaxX], MaxY = midY], capacity = node[capacity], shapes = {}, children = null],
                    // NE
                    [envelope = [MinX = midX, MinY = midY, MaxX = b[MaxX], MaxY = b[MaxY]], capacity = node[capacity], shapes = {}, children = null],
                    // NW
                    [envelope = [MinX = b[MinX], MinY = midY, MaxX = midX, MaxY = b[MaxY]], capacity = node[capacity], shapes = {}, children = null]
                }
            in 
                Record.AddField(node, "children", children)
        ),
        type function (node as TQuadTreeNode) as TQuadTreeNode
    ),
    QuadTreeCreate = Value.ReplaceType(
        (capacity as number, initialEnvelope as nullable record) as record => (
            [
                root = [
                    shapes = {},
                    capacity = capacity,
                    children = null,
                    envelope = initialEnvelope ?? [
                        MinX = 0,
                        MinY = 0,
                        MaxX = 0,
                        MaxY = 0
                    ]
                ],
                capacity = capacity
            ]
        ),
        type function (capacity as number, initialEnvelope as nullable TShapeEnvelope) as TQuadTree
    ),
    QuadTreeNodeInsert = Value.ReplaceType(
        (node as record, shape as record, isRoot as logical) as record => (
            let
                bNode = node[envelope],
                bShape = shape[Envelope],
                intersects = QuadTreeBoxesIntersect(bNode, bShape),

                // Inserts the shape into the single child it intersects; otherwise keeps it on this node.
                // @param state - The quadtree node being updated (its children/shapes may change)
                // @param s - The shape to insert or retain at this node
                insertIntoChildrenOrKeep = (state as record, s as record) as record => (
                    let
                        bbox = s[Envelope],
                        children = state[children],
                        containedIndexes = List.Select(
                            List.Positions(children),
                            (i as number) as logical => QuadTreeBoxesIntersect(bbox, children{i}[envelope])
                        )
                    in
                        if List.Count(containedIndexes) = 1 then
                            let
                                idx = List.First(containedIndexes),
                                updatedChildren = List.Transform(
                                    {0..List.Count(children)-1},
                                    (i as number) as record => if i = idx then @QuadTreeNodeInsert(children{i}, s, false) else children{i}
                                )
                            in
                                Record.TransformFields(state, {"children", each updatedChildren})
                        else
                            Record.TransformFields(state, {"shapes", each _ & {s}})
                ),
                expandEnvelope = (p as record, t as record) as record => [
                    MinX = List.Min({p[MinX], t[MinX]}),
                    MinY = List.Min({p[MinY], t[MinY]}),
                    MaxX = List.Max({p[MaxX], t[MaxX]}),
                    MaxY = List.Max({p[MaxY], t[MaxY]})
                ],
                createExpandedParent = (node as record, shape as record) as record => (
                    let
                        b1 = node[envelope],
                        b2 = shape[Envelope],
						// Build a square parent envelope starting from the existing node,
						// then double its size and shift until it fully contains the shape's envelope
						w1 = b1[MaxX] - b1[MinX],
						h1 = b1[MaxY] - b1[MinY],
						baseSize = if w1 > h1 then w1 else h1,
						size0 = if baseSize = 0 then 1 else baseSize,
						parent0 = [
							MinX = b1[MinX],
							MinY = b1[MinY],
							MaxX = b1[MinX] + size0,
							MaxY = b1[MinY] + size0
						],
						contains = (p as record, t as record) as logical =>
							(t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY]),
						expand = (p as record, size as number) as record =>
							if @contains(p, b2) then p else
							let
								dx = if b2[MinX] < p[MinX] then -1 else if b2[MaxX] > p[MaxX] then 1 else 0,
								dy = if b2[MinY] < p[MinY] then -1 else if b2[MaxY] > p[MaxY] then 1 else 0,
								p2 = [
									MinX = p[MinX] - (if dx = -1 then size else 0),
									MinY = p[MinY] - (if dy = -1 then size else 0),
									MaxX = p[MaxX] + (if dx = 1 then size else 0),
									MaxY = p[MaxY] + (if dy = 1 then size else 0)
								]
							in
								@expand(p2, size * 2),
						parentEnvelope = expand(parent0, size0),
						midX = (parentEnvelope[MinX] + parentEnvelope[MaxX]) / 2,
						midY = (parentEnvelope[MinY] + parentEnvelope[MaxY]) / 2,
						quadrants = {
							[MinX = parentEnvelope[MinX], MinY = parentEnvelope[MinY], MaxX = midX, MaxY = midY], // SW
							[MinX = midX, MinY = parentEnvelope[MinY], MaxX = parentEnvelope[MaxX], MaxY = midY], // SE
							[MinX = midX, MinY = midY, MaxX = parentEnvelope[MaxX], MaxY = parentEnvelope[MaxY]], // NE
							[MinX = parentEnvelope[MinX], MinY = midY, MaxX = midX, MaxY = parentEnvelope[MaxY]]  // NW
						},
						cx = (b1[MinX] + b1[MaxX]) / 2,
						cy = (b1[MinY] + b1[MaxY]) / 2,
						qx = if cx < midX then 0 else 1,
						qy = if cy < midY then 0 else 1,
						idx = if qx = 0 and qy = 0 then 0 else if qx = 1 and qy = 0 then 1 else if qx = 1 and qy = 1 then 2 else 3,
						makeChild = (env as record) as record => [envelope = env, capacity = node[capacity], shapes = {}, children = null],
						baseChildren = List.Transform(quadrants, each @makeChild(_)),
						updatedChildren = List.Transform({0..3}, (i as number) as record => if i = idx then Record.TransformFields(node, {"envelope", each quadrants{i}}) else baseChildren{i}),
						newParent = [
							envelope = parentEnvelope,
							capacity = node[capacity],
							shapes = {},
							children = updatedChildren
						],
						withShape = insertIntoChildrenOrKeep(newParent, shape)
					in
						withShape
				),
                result =
                    //If the shape does not intersect the node, return the node unchanged
                    if not intersects then
                        //If the node is the root, then we need to expand the envelope to include the shape either by creating a new parent (if the capacity is exceeded) or expanding the existing one, then insert
                        if isRoot then
                            if List.Count(node[shapes]) < node[capacity] then
                                let
                                    expanded = Record.TransformFields(node, {"envelope", each @expandEnvelope(_, bShape)}),
                                    reinserted = @QuadTreeNodeInsert(expanded, shape, true)
                                in
                                    reinserted
                            else
                                //Create a new parent and expand the envelope to include the shape
                                createExpandedParent(node, shape)
                        else
                            node
                    //If the node has no children
                    else if node[children] = null then
                        //If the node has capacity, add the shape to the node
                        if List.Count(node[shapes]) < node[capacity] then
                            Record.TransformFields(node, {"shapes", each _ & {shape}})
                        //If the node does not have capacity, subdivide the node and redistribute the shapes
                        else
                            let
                                subdivided = QuadTreeSubdivideNode(node),
                                allShapes = node[shapes] & {shape},
                                reshaped = List.Accumulate(allShapes, subdivided, insertIntoChildrenOrKeep)
                            in
                                reshaped
                    //If the node has children
                    else
                        //Insert the shape into the appropriate child
                        insertIntoChildrenOrKeep(node, shape)
            in
                result
        ),
        type function (node as TQuadTreeNode, shape as TShape, isRoot as logical) as TQuadTreeNode
    ),
    
    QuadTreeInsert = Value.ReplaceType(
        (quadTree as record, shape as record) as record => (
            let
                result = QuadTreeNodeInsert(quadTree[root], shape, true),
                updatedRoot = Record.TransformFields(quadTree, {"root", each result})
            in
                updatedRoot
        ),
        type function (quadTree as TQuadTree, shape as TShape) as TQuadTree
    ),

    QuadTreeOperatorIntersect = Value.ReplaceType(
        (candidate as record, q as record) as logical => (
            QuadTreeBoxesIntersect(candidate[Envelope], q[Envelope])
        ),
        TQuadTreeQueryOperator
    ),

    QuadTreeOperatorContains = Value.ReplaceType(
        (candidate as record, q as record) as logical => (
            let
                p = candidate[Envelope],
                t = q[Envelope]
            in
                (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY])
        ),
        TQuadTreeQueryOperator
    ),

    QuadTreeOperatorWithin = Value.ReplaceType(
        (candidate as record, q as record) as logical => (
            let
                p = q[Envelope],
                t = candidate[Envelope]
            in
                (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY])
        ),
        TQuadTreeQueryOperator
    ),
    QuadTreeNodeQuery = Value.ReplaceType(
        (node as record, shape as record, callback as function) as list => (
            let
                queryEnvelope = shape[Envelope],
                nodeEnvelope = node[envelope],
                // prune by envelope intersection
                nodeIntersects = QuadTreeBoxesIntersect(nodeEnvelope, queryEnvelope),

                // envelope helpers
                envContains = (p as record, t as record) as logical =>
                    (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY]),
                envWithin = (x as record, y as record) as logical => @envContains(y, x),

                // test shapes at this node
                fromNode = if nodeIntersects then List.Select(node[shapes], each callback(_, shape)) else {},

                // recurse into children that intersect
                fromChildren = if nodeIntersects and node[children] <> null then
                    let
                        kids = node[children],
                        lists = List.Transform(kids, (c as record) as list => if QuadTreeBoxesIntersect(c[envelope], queryEnvelope) then @QuadTreeNodeQuery(c, shape, callback) else {})
                    in
                        List.Combine(lists)
                else
                    {}
            in
                fromNode & fromChildren
        ),
        type function (node as TQuadTreeNode, shape as TShape, callback as function) as list
    ),
    QuadTreeQuery = Value.ReplaceType(
        (quadTree as record, shape as record, callback as function) as list => (
            QuadTreeNodeQuery(quadTree[root], shape, callback)
        ),
        type function (quadTree as TQuadTree, shape as TShape, operator as TQuadTreeQueryOperator) as list
    ),
    QuadTreeNodePrune = Value.ReplaceType(
        (node as record, ids as list) as record => (
            let
                fromNode = if List.MatchesAll(node[shapes], each List.Contains(ids, Record.Field(_, "__rowid__"))) then node else {},
                fromChildren = if node[children] <> null then List.Transform(node[children], (c as record) as record => @QuadTreeNodePrune(c, ids)) else {},
                newNode = Record.TransformFields(node, {"shapes", each fromNode, "children", each fromChildren})
            in
                newNode
        ),
        type function (node as TQuadTreeNode, ids as list) as TQuadTreeNode
    ),
    LayerCreateBlank = Value.ReplaceType(
        (geometryColumn as nullable text, capacity as nullable number) as record => (
            let
                geomCol = geometryColumn ?? "shape",
                tbl = #table({"__rowid__", geomCol}, {})
            in
            [
                table = tbl,
                geometryColumn = geomCol,
                queryLayer = QuadTreeCreate(capacity ?? 10, null)
            ]
        ),
        type function (geometryColumn as nullable text, capacity as nullable number) as TLayer
    ),
    LayerCreateFromTable = Value.ReplaceType(
        (tbl as table, geometryColumn as text) as record => (
            let
                tblWithId = EnsureRowIdColumn(tbl),
                qtCapacity = Table.RowCount(tblWithId) / 10,
                qt = QuadTreeCreate(qtCapacity, null),
                shapesWithIds = Table.TransformRows(
                    tblWithId,
                    (r as record) as record =>
                        let
                            s = Record.Field(r, geometryColumn),
                            sid = Record.Field(r, "__rowid__"),
                            s2 = if Record.HasFields(s, "__rowid__") then Record.TransformFields(s, {"__rowid__", each sid}) else Record.AddField(s, "__rowid__", sid)
                        in
                            s2
                ),
                inserted = List.Accumulate(shapesWithIds, qt, QuadTreeInsert)
            in 
                [
                    table = tblWithId,
                    geometryColumn = geometryColumn,
                    queryLayer = inserted
                ]
        ),
        type function (tbl as table, geometryColumn as text) as TLayer
    ),
	LayerInsertRows = Value.ReplaceType(
		(layer as record, rows as list) as record => (
			let
				baseTable = EnsureRowIdColumn(layer[table]),
				nextId0 = if Table.RowCount(baseTable) = 0 then 0 else 1 + List.Max(Table.Column(baseTable, "__rowid__")),
				// 1) Ensure __rowid__ present on all rows, assigning sequentially from nextId0 when missing
				assignIds = (state as record, r as record) as record => (
					let
						rid = state[nextId],
						rWithId = if Record.HasFields(r, "__rowid__") then Record.TransformFields(r, {"__rowid__", each rid}) else Record.AddField(r, "__rowid__", rid),
						nextId1 = state[nextId] + 1
					in
						[rowsOut = state[rowsOut] & {rWithId}, nextId = nextId1]
				),
				assigned = List.Accumulate(rows ?? {}, [rowsOut = {}, nextId = nextId0], assignIds),
				rowsWithIds = assigned[rowsOut],
				// 2) Ensure shapes have corresponding __rowid__
				shapesWithIds = List.Transform(
					rowsWithIds,
					(r as record) as record => (
						let
							s0 = Record.Field(r, layer[geometryColumn]),
							rid = Record.Field(r, "__rowid__"),
							s1 = if Record.HasFields(s0, "__rowid__") then Record.TransformFields(s0, {"__rowid__", each rid}) else Record.AddField(s0, "__rowid__", rid)
						in
							s1
					)
				),
				// 3) Recursively add shapes to quadtree (one-by-one, preserving insert semantics)
				qt1 = List.Accumulate(shapesWithIds, layer[queryLayer], QuadTreeInsert),
				// 4) Insert all prepared rows into the table in a single batch
				newTable = if List.Count(rowsWithIds) = 0 then baseTable else Table.InsertRows(baseTable, 0, rowsWithIds)
			in
				[
					table = newTable,
					geometryColumn = layer[geometryColumn],
					queryLayer = qt1
				]
		),
		type function (layer as TLayer, rows as list) as TLayer
	),
    LayerQuerySpatial = Value.ReplaceType(
        (layer as record, shape as record, gisOperator as record) as record => (
            let
                shapes = QuadTreeQuery(layer[queryLayer], shape, gisOperator),
                ids = List.Transform(List.Select(shapes, each Record.HasFields(_, "__rowid__") and (Record.Field(_, "__rowid__") <> null)), each Record.Field(_, "__rowid__")),
                idsDistinct = List.Distinct(ids),
                selected = Table.SelectRows(layer[table], (r as record) as logical => List.Contains(idsDistinct, Record.Field(r, "__rowid__"))),
                filteredQuadTree = [
                    root = QuadTreeNodePrune(layer[queryLayer][root], idsDistinct),
                    capacity = layer[queryLayer][capacity]
                ]
            in
                [
                    table = selected,
                    geometryColumn = layer[geometryColumn],
                    queryLayer = filteredQuadTree
                ]
        ),
        type function (layer as TLayer, shape as TShape, gisOperator as TQuadTreeQueryOperator) as TLayer
    ),
    LayerQueryRelational = Value.ReplaceType(
        (layer as record, operator as function) as record => (
            let
                selected = Table.SelectRows(layer[table], each operator(_)),
                selectedIDs = List.Transform(selected, each Record.Field(_, "__rowid__")),
                filteredQuadTree = [
                    root = QuadTreeNodePrune(layer[queryLayer][root], selectedIDs),
                    capacity = layer[queryLayer][capacity]
                ]
            in
                [
                    table = selected,
                    geometryColumn = layer[geometryColumn],
                    queryLayer = filteredQuadTree
                ]
        ),
        type function (layer as TLayer, operator as TRowQueryOperator) as TLayer
    ),
    LayerJoinSpatial = Value.ReplaceType(
        (layer1 as record, layer2 as record, gisOperator as nullable function, joinType as nullable text) as record => (
            let
                actualJoinType = joinType ?? "Inner",
                actualGisOperator = gisOperator ?? QuadTreeOperatorIntersect,
                table1 = layer1[table],
                table2 = layer2[table],
                geomCol1 = layer1[geometryColumn],
                geomCol2 = layer2[geometryColumn],
                
                // For each row in layer1, find matching rows in layer2
                layer1Matches = Table.TransformRows(
                    table1,
                    (r1 as record) as record => (
                        let
                            shape1 = Record.Field(r1, geomCol1),
                            matchingShapes = QuadTreeQuery(layer2[queryLayer], shape1, actualGisOperator),
                            matchingIds = List.Transform(
                                List.Select(matchingShapes, each Record.HasFields(_, "__rowid__") and (Record.Field(_, "__rowid__") <> null)),
                                each Record.Field(_, "__rowid__")
                            ),
                            matchingRows = Table.SelectRows(table2, (r2 as record) as logical => List.Contains(matchingIds, Record.Field(r2, "__rowid__")))
                        in
                            [row1 = r1, matchingRows = matchingRows, shape1 = shape1]
                    )
                ),
                
                // Build result based on join type
                resultRows = (
                    if actualJoinType = "Inner" then
                        // Inner join: only rows with matches
                        List.Combine(
                            List.Transform(
                                layer1Matches,
                                (match as record) as list => (
                                    let
                                        r1 = match[row1],
                                        matches = Table.ToRecords(match[matchingRows])
                                    in
                                        if List.Count(matches) > 0 then
                                            List.Transform(matches, (r2 as record) as record => [
                                                layer1 = r1,
                                                layer2 = r2,
                                                shape = match[shape1]
                                            ])
                                        else
                                            {}
                                )
                            )
                        )
                    else if actualJoinType = "Left Outer" then
                        // Left Outer: all rows from layer1
                        List.Combine(
                            List.Transform(
                                layer1Matches,
                                (match as record) as list => (
                                    let
                                        r1 = match[row1],
                                        matches = Table.ToRecords(match[matchingRows])
                                    in
                                        if List.Count(matches) > 0 then
                                            List.Transform(matches, (r2 as record) as record => [
                                                layer1 = r1,
                                                layer2 = r2,
                                                shape = match[shape1]
                                            ])
                                        else
                                            {[
                                                layer1 = r1,
                                                layer2 = null,
                                                shape = match[shape1]
                                            ]}
                                )
                            )
                        )
                    else if actualJoinType = "Right Outer" then
                        // Right Outer: all rows from layer2
                        let
                            // Get all layer2 rowids that were matched
                            matchedLayer2Ids = List.Distinct(
                                List.Combine(
                                    List.Transform(
                                        layer1Matches,
                                        (match as record) as list => (
                                            Table.Column(match[matchingRows], "__rowid__")
                                        )
                                    )
                                )
                            ),
                            // Rows with matches
                            matchedRows = List.Combine(
                                List.Transform(
                                    layer1Matches,
                                    (match as record) as list => (
                                        let
                                            r1 = match[row1],
                                            matches = Table.ToRecords(match[matchingRows])
                                        in
                                            List.Transform(matches, (r2 as record) as record => [
                                                layer1 = r1,
                                                layer2 = r2,
                                                shape = Record.Field(r2, geomCol2)
                                            ])
                                    )
                                )
                            ),
                            // Unmatched rows from layer2
                            unmatchedRows = List.Transform(
                                Table.SelectRows(table2, (r2 as record) as logical => not List.Contains(matchedLayer2Ids, Record.Field(r2, "__rowid__"))),
                                (r2 as record) as record => [
                                    layer1 = null,
                                    layer2 = r2,
                                    shape = Record.Field(r2, geomCol2)
                                ]
                            )
                        in
                            matchedRows & unmatchedRows
                    else  if actualJoinType = "Full Outer" then
                        let
                            // Get all layer2 rowids that were matched
                            matchedLayer2Ids = List.Distinct(
                                List.Combine(
                                    List.Transform(
                                        layer1Matches,
                                        (match as record) as list => (
                                            Table.Column(match[matchingRows], "__rowid__")
                                        )
                                    )
                                )
                            ),
                            // Rows with matches and layer1 without matches
                            layer1Results = List.Combine(
                                List.Transform(
                                    layer1Matches,
                                    (match as record) as list => (
                                        let
                                            r1 = match[row1],
                                            matches = Table.ToRecords(match[matchingRows])
                                        in
                                            if List.Count(matches) > 0 then
                                                List.Transform(matches, (r2 as record) as record => [
                                                    layer1 = r1,
                                                    layer2 = r2,
                                                    shape = match[shape1]
                                                ])
                                            else
                                                {[
                                                    layer1 = r1,
                                                    layer2 = null,
                                                    shape = match[shape1]
                                                ]}
                                    )
                                )
                            ),
                            // Unmatched rows from layer2
                            unmatchedLayer2 = List.Transform(
                                Table.SelectRows(table2, (r2 as record) as logical => not List.Contains(matchedLayer2Ids, Record.Field(r2, "__rowid__"))),
                                (r2 as record) as record => [
                                    layer1 = null,
                                    layer2 = r2,
                                    shape = Record.Field(r2, geomCol2)
                                ]
                            )
                        in
                            layer1Results & unmatchedLayer2
                    else
                        error "Invalid join type"
                ),

                // Create result table with __rowid__ column
                resultTable0 = Table.FromRecords(resultRows, type table [layer1 = any, layer2 = any, shape = any]),
                resultTable = Table.AddIndexColumn(resultTable0, "__rowid__", 0, 1, Int64.Type),
                resultTableReordered = Table.ReorderColumns(resultTable, {"__rowid__", "layer1", "layer2", "shape"}),
                
                // Build quadtree for the result
                qtCapacity = if List.Count(resultRows) > 100 then List.Count(resultRows) / 10 else 10,
                qt = QuadTreeCreate(qtCapacity, null),
                shapesWithIds = Table.TransformRows(
                    resultTableReordered,
                    (r as record) as record =>
                        let
                            s = r[shape],
                            sid = r[__rowid__],
                            s2 = if Record.HasFields(s, "__rowid__") then Record.TransformFields(s, {"__rowid__", each sid}) else Record.AddField(s, "__rowid__", sid)
                        in
                            s2
                ),
                inserted = List.Accumulate(shapesWithIds, qt, QuadTreeInsert)
            in
                [
                    table = resultTableReordered,
                    geometryColumn = "shape",
                    queryLayer = inserted
                ]
        ),
        type function (layer1 as TLayer, layer2 as TLayer, gisOperator as nullable TQuadTreeQueryOperator, joinType as nullable text) as TLayer
    )
in
    [
        gisShapeCreateFromWKT = ShapeCreateFromWKT,
        gisLayerCreateBlank = LayerCreateBlank,
        gisLayerCreateFromTable = LayerCreateFromTable,
        gisLayerQuerySpatial = LayerQuerySpatial,
        gisLayerQueryRelational = LayerQueryRelational,
        gisLayerQueryOperators = [
            gisIntersects = QuadTreeOperatorIntersect,
            gisContains = QuadTreeOperatorContains,
            gisWithin = QuadTreeOperatorWithin,
            gisQueryOperatorType = TQuadTreeQueryOperator //Provided for custom operators
        ],
		gisLayerInsertRows = LayerInsertRows,
		gisLayerJoinSpatial = LayerJoinSpatial
    ]



