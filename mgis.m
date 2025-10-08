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
    LayerInsertRow = Value.ReplaceType(
        (layer as record, row as record) as record => (
            let
                // ensure table has __rowid__
                baseTable = EnsureRowIdColumn(layer[table]),
                nextId = if Table.RowCount(baseTable) = 0 then 0 else 1 + List.Max(Table.Column(baseTable, "__rowid__")),
                rowHasId = Record.HasFields(row, "__rowid__"),
                rowId = if rowHasId then Record.Field(row, "__rowid__") else nextId,
                rowWithId = if rowHasId then row else Record.AddField(row, "__rowid__", rowId),
                shape0 = Record.Field(rowWithId, layer[geometryColumn]),
                shape1 = if Record.HasFields(shape0, "__rowid__") then Record.TransformFields(shape0, {"__rowid__", each rowId}) else Record.AddField(shape0, "__rowid__", rowId),
                inserted = QuadTreeInsert(layer[queryLayer], shape1),
                newTable = Table.InsertRows(baseTable, 0, {rowWithId})
            in
                [
                    table = newTable,
                    geometryColumn = layer[geometryColumn],
                    queryLayer = inserted
                ]
        ),
        type function (layer as TLayer, row as record) as TLayer
    ),
    LayerQuery = Value.ReplaceType(
        (layer as record, shape as record, operator as record) as list => (
            let
                shapes = QuadTreeQuery(layer[queryLayer], shape, operator),
                ids = List.Transform(List.Select(shapes, each Record.HasFields(_, "__rowid__") and (Record.Field(_, "__rowid__") <> null)), each Record.Field(_, "__rowid__")),
                idsDistinct = List.Distinct(ids),
                selected = Table.SelectRows(layer[table], (r as record) as logical => List.Contains(idsDistinct, Record.Field(r, "__rowid__")))
            in
                Table.ToRecords(selected)
        ),
        type function (layer as TLayer, shape as TShape, operator as TQuadTreeQueryOperator) as list
    )
in
    [
        gisShapeCreateFromWKT = ShapeCreateFromWKT,
        gisLayerCreateBlank = LayerCreateBlank,
        gisLayerCreateFromTable = LayerCreateFromTable,
        gisLayerQuery = LayerQuery,
        gisLayerQueryOperators = [
            gisIntersects = QuadTreeOperatorIntersect,
            gisContains = QuadTreeOperatorContains,
            gisWithin = QuadTreeOperatorWithin,
            gisQueryOperatorType = TQuadTreeQueryOperator //Provided for custom operators
        ],
        gisLayerInsertRow = LayerInsertRow
    ]



