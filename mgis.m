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
        //To ensure this is a TShape, not some other record, we add this field. It won't be used for any other purpose. Just a sanity check.
        __TShapeIdentifier__ = null,
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
    ShapeCreatePointFromLatLng = Value.ReplaceType(
        (lat as number, lng as number) as record => (
            let
                baseGeometry = GeometryPoint.From(lng, lat)
            in [
                __TShapeIdentifier__ = null,
                Kind = baseGeometry[Kind],
                Geometry = baseGeometry,
                Envelope = privGetEnvelope(baseGeometry),
                __rowid__ = null
            ]
        ),
        type function (lat as number, lng as number) as TShape
    ),
    ShapeCreateFromWKT = Value.ReplaceType(
        (wkt as text) as record => (
            let 
                baseGeometry = Geometry.FromWellKnownText(wkt),
                envelope = privGetEnvelope(baseGeometry)
            in [
                __TShapeIdentifier__ = null,
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
    TQuadTreeQueryOperator = type [
        // Required: per‑candidate callback
        onCandidate = function (candidate as TShape, query as TShape) as any,

        // Optional: aggregate child/node results
        combine = nullable function (childResults as list) as any,

        // Optional: early‑exit / pruning rule
        continueSearch = nullable function (currentBest as any) as logical,

        // Optional: additional columns that should be present in results
        additionalColumns = nullable {text}
    ],
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
                if Record.HasFields(node, {"children"}) then
                    Record.TransformFields(node, {"children", each children})
                else
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
                _ = if not Record.HasFields(shape, {"__TShapeIdentifier__"}) then error "Shape is not a TShape" else null,
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
                _ = if not Record.HasFields(shape, {"__TShapeIdentifier__"}) then error "Shape is not a TShape" else null,
                result = QuadTreeNodeInsert(quadTree[root], shape, true),
                updatedRoot = Record.TransformFields(quadTree, {"root", each result})
            in
                updatedRoot
        ),
        type function (quadTree as TQuadTree, shape as TShape) as TQuadTree
    ),

    //Candidate envelope intersects query envelope
    QuadTreeOperatorIntersect = Value.ReplaceType([
        onCandidate = (candidate as record, query as record) as list =>
            let
                _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
                a = candidate[Envelope],
                b = query[Envelope],
                intersects =
                    not (
                        a[MaxX] < b[MinX] or
                        a[MinX] > b[MaxX] or
                        a[MaxY] < b[MinY] or
                        a[MinY] > b[MaxY]
                    )
            in
                if intersects then { [shape=candidate] } else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),

    //Candidate envelope fully contains query envelope
    QuadTreeOperatorContains = Value.ReplaceType(
        [
            onCandidate = (candidate as record, query as record) as list =>
                let
                    _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                    _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
                    p = candidate[Envelope],
                    t = query[Envelope],
                    contains =
                        (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and
                        (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY])
                in
                    if contains then { [shape=candidate] } else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),
    
    //Candidate envelope fully inside query envelope
	QuadTreeOperatorWithin = Value.ReplaceType(
	    [
	        onCandidate = (candidate as record, query as record) as list =>
	            let
                    _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                    _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
	                p = query[Envelope],
	                t = candidate[Envelope],
	                within =
	                    (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and
	                    (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY])
	            in
	                if within then { [shape=candidate] } else {},
	        combine = null,
	        continueSearch = null,
	        additionalColumns = null
	    ],
	    TQuadTreeQueryOperator
	),

    //Nearest neighbor
    QuadTreeOperatorNearestN = Value.ReplaceType(
        (k as number) as record =>
            [
                onCandidate = (candidate as record, query as record) as record =>
                    let
                        _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                        _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
                        e1  = candidate[Envelope],
                        e2  = query[Envelope],
                        cx1 = (e1[MinX] + e1[MaxX]) / 2,
                        cy1 = (e1[MinY] + e1[MaxY]) / 2,
                        cx2 = (e2[MinX] + e2[MaxX]) / 2,
                        cy2 = (e2[MinY] + e2[MaxY]) / 2,
                        dist = Number.Sqrt((cx1 - cx2)*(cx1 - cx2) + (cy1 - cy2)*(cy1 - cy2))
                    in
                        [ shape = candidate, dist = dist ],

                combine = (lists as list) as list =>
                    let
                        all   = List.Combine(lists),
                        sorted = List.Sort(all, each _[dist]),
                        bestN  = List.FirstN(sorted, k)
                    in
                        bestN,

                continueSearch = (partial as any) as logical => 
                    // stop when we already collected exactly k = 1 neighbours with dist = 0
                    if Value.Is(partial, type list) and List.Count(partial) = k then false else true,

                additionalColumns = {"dist"}
            ],
            type function (k as number) as TQuadTreeQueryOperator
    ),

    //Nearest neighbor
    QuadTreeOperatorNearest = QuadTreeOperatorNearestN(1),

    NormalizeList = (value as any) as list =>
        if value = null then
            {}
        else if Value.Is(value, type list) then
            value
        else
            { value },

    // Ensure all records have the same schema by adding missing columns as null
    NormalizeRecords = (records as list, additionalColumns as nullable list) as list =>
        if additionalColumns = null or List.Count(additionalColumns) = 0 then
            records
        else
            List.Transform(
                records,
                (r as record) as record =>
                    let
                        existingFields = Record.FieldNames(r),
                        missingFields = List.RemoveItems(additionalColumns, existingFields),
                        nullFields = List.Transform(missingFields, each [Name = _, Value = null]),
                        additionalRecord = Record.FromList(List.Transform(nullFields, each _[Value]), List.Transform(nullFields, each _[Name]))
                    in
                        Record.Combine({r, additionalRecord})
            ),

    //Query a node based on a spatial relationship to a supplied shape
    QuadTreeNodeQuery = Value.ReplaceType(
        (node as record, shape as record, operator as record) as any =>
            let
                _ = if not Record.HasFields(shape, {"__TShapeIdentifier__"})
                        then error "Shape is not a TShape"
                        else null,

                onCandidate    = operator[onCandidate],
                combine        = if operator[combine] <> null
                                    then operator[combine]
                                    else List.Combine,
                continueSearch = if operator[continueSearch] <> null
                                    then operator[continueSearch]
                                    else (x as any) => true,

                queryEnvelope = shape[Envelope],
                nodeEnvelope  = node[envelope],
                nodeIntersects = QuadTreeBoxesIntersect(nodeEnvelope, queryEnvelope),

                //------------------------------------------
                // Candidate results at this node
                //------------------------------------------
                fromNodeRaw =
                    if nodeIntersects
                        then List.Transform(node[shapes], each onCandidate(_, shape))
                        else {},
                fromNode = NormalizeList(List.Combine(List.Transform(fromNodeRaw, each NormalizeList(_)))),

                //------------------------------------------
                // Recurse into children
                //------------------------------------------
                fromChildrenRaw =
                    if nodeIntersects and node[children] <> null and continueSearch(fromNode)
                        then List.Transform(
                                node[children],
                                (c as record) =>
                                    if QuadTreeBoxesIntersect(c[envelope], queryEnvelope)
                                        then @QuadTreeNodeQuery(c, shape, operator)
                                        else {})
                        else {},
                fromChildren = NormalizeList(List.Combine(List.Transform(fromChildrenRaw, each NormalizeList(_)))),

                //------------------------------------------
                // Combine results safely
                //------------------------------------------
                result = combine({ fromNode, fromChildren })
            in
                result,

        type function (node as TQuadTreeNode, shape as TShape, operator as TQuadTreeQueryOperator) as any
    ),
    
	QuadTreeQuery = Value.ReplaceType(
        (qt as record, shape as record, op as record) as list =>
            let
                res = QuadTreeNodeQuery(qt[root], shape, op),
                listOut = NormalizeList(res)
            in
                listOut,
        type function (qt as TQuadTree, shape as TShape, op as TQuadTreeQueryOperator) as list
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

    //Create a blank layer
    //@param geometryColumn - The column that should contain the geometry
    //@param capacity - The capacity of the quadtree
    //@return The created layer, the table of which has the same columns as the original table.
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

    //Create a layer from a table
    //@param tbl - The table to create the layer from
    //@param geometryColumn - The column that contains the geometry
    //@return The created layer, the table of which has the same columns as the original table.
    LayerCreateFromTable = Value.ReplaceType(
        (tbl as table, geometryColumn as text) as record => (
            let
                _ = if not Table.HasColumns(tbl, {geometryColumn}) then error "Table does not have the geometry column. Please create the column with the relevant `gisShapeCreateFrom...` functions." else null,
                _2 = if not Table.MatchesAllRows(tbl, each Record.HasFields(_, {"__TShapeIdentifier__"})) then error "Table geometry column does not contain `TShape`s. Please recreate the column with the relevant `gisShapeCreateFrom...` functions." else null,
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

    //Insert rows into a layer
    //@param layer - The layer to insert the rows into
    //@param rows - The rows to insert
    //@return The updated layer, the table of which has the same columns as the original layer.
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

    //Query a layer based on a spatial relationship to a supplied shape
    //@param layer - The layer to query
    //@param shape - The shape to query against
    //@param gisOperator - A function that takes two shapes and returns a logical value indicating if they meet the spatial relationship.
    //@return The queried layer, the table of which has the same columns as the original layer.
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

    //Query a layer based on a row-wise relational operator
    //@param layer - The layer to query
    //@param operator - The relational operator to use for the query.
    //@return The queried layer, the table of which has the same columns as the original layer.
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

    //Join two layers together based on a spatial relationship
    //@param layer1 - The first layer to join
    //@param layer2 - The second layer to join
    //@param gisOperator - The spatial relationship to use for the join. If not provided, the default is Intersect.
    //                     Remark: Read like "(shapes of layer1) within (shapes of layer2)"
    //@param joinType - The type of join to perform. If not provided, the default is "Inner".
    //                  Remark: Supported types are "Inner", "Left Outer", "Right Outer", "Full Outer"
    //@return The joined layer, the table of which has 4 columns: __rowid__, layer1, layer2, shape. Where layer1 and layer2 are the row objects from the original layers, and shape is the shape of the row from the original layer.
    LayerJoinSpatial = Value.ReplaceType(
        (layer1 as record, layer2 as record, gisOperator as nullable record, joinType as nullable text) as record => (
            let
                actualJoinType = joinType ?? "Inner",
                actualGisOperator = gisOperator ?? [onCandidate = QuadTreeOperatorIntersect],
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
                            matchingIds =
                                List.Transform(
                                    matchingShapes,
                                    each _[shape][__rowid__]
                                ),
                            matchingRows = Table.SelectRows(table2, (r2 as record) as logical => List.Contains(matchingIds, Record.Field(r2, "__rowid__")))
                        in
                            [row1 = r1, matchingRows = matchingRows, matchingShapes = matchingShapes, shape1 = shape1]
                    )
                ),
                
                // Build result based on join type
                resultRows = (
                    if actualJoinType = "Inner" then
                        // Inner join: only rows with matches
                        List.Combine(
                            List.Transform(
                                layer1Matches,
                                (match as record) as list =>
                                    let
                                        r1 = match[row1],
                                        matches = Table.ToRecords(match[matchingRows]),
                                        shapes = match[matchingShapes]
                                    in
                                        if List.Count(matches) > 0 then
                                            List.Transform(
                                                matches,
                                                (r2 as record) as record =>
                                                    let
                                                        r2id = r2[__rowid__],
                                                        shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                        shapeExtras = if shapeData <> null then Record.RemoveFields(shapeData, {"shape"}) else [],
                                                        actualShape = if shapeData <> null then shapeData[shape] else null
                                                    in
                                                        Record.Combine({
                                                            [ 
                                                                layer1 = r1,
                                                                layer2 = r2,
                                                                shape  = actualShape
                                                            ],
                                                            shapeExtras
                                                        })
                                            )
                                        else
                                            {}
                            )
                        )

                    else if actualJoinType = "Left Outer" then
                        // Left Outer: all rows from layer1
                        List.Combine(
                            List.Transform(
                                layer1Matches,
                                (match as record) as list =>
                                    let
                                        r1 = match[row1],
                                        matches = Table.ToRecords(match[matchingRows]),
                                        shapes = match[matchingShapes]
                                    in
                                        if List.Count(matches) > 0 then
                                            List.Transform(
                                                matches,
                                                (r2 as record) as record =>
                                                    let
                                                        r2id = r2[__rowid__],
                                                        shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                        shapeExtras = if shapeData <> null then Record.RemoveFields(shapeData, {"shape"}) else [],
                                                        actualShape = if shapeData <> null then shapeData[shape] else null
                                                    in
                                                        Record.Combine({
                                                            [ 
                                                                layer1 = r1,
                                                                layer2 = r2,
                                                                shape  = actualShape
                                                            ],
                                                            shapeExtras
                                                        })
                                            )
                                        else
                                            {
                                                [ 
                                                    layer1 = r1,
                                                    layer2 = null,
                                                    shape  = match[shape1]
                                                ]
                                            }
                            )
                        )

                    else if actualJoinType = "Right Outer" then
                        // Right Outer: all rows from layer2
                        let
                            matchedLayer2Ids =
                                List.Distinct(
                                    List.Combine(
                                        List.Transform(
                                            layer1Matches,
                                            (match as record) as list =>
                                                Table.Column(match[matchingRows], "__rowid__")
                                        )
                                    )
                                ),
                            // Rows with matches
                            matchedRows =
                                List.Combine(
                                    List.Transform(
                                        layer1Matches,
                                        (match as record) as list =>
                                            let
                                                r1 = match[row1],
                                                matches = Table.ToRecords(match[matchingRows]),
                                                shapes = match[matchingShapes]
                                            in
                                                List.Transform(
                                                    matches,
                                                    (r2 as record) as record =>
                                                        let
                                                            r2id = r2[__rowid__],
                                                            shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                            shapeExtras = if shapeData <> null then Record.RemoveFields(shapeData, {"shape"}) else [],
                                                            extras = Record.RemoveFields(r2, {"shape", "__rowid__"}),
                                                            actualShape = if shapeData <> null then shapeData[shape] else null
                                                        in
                                                            Record.Combine({
                                                                [ 
                                                                    layer1 = r1,
                                                                    layer2 = r2,
                                                                    shape  = actualShape
                                                                ],
                                                                extras,
                                                                shapeExtras
                                                            })
                                                )
                                    )
                                ),
                            // Unmatched rows from layer2
                            unmatchedRows =
                                List.Transform(
                                    Table.SelectRows(
                                        table2,
                                        (r2 as record) as logical =>
                                            not List.Contains(matchedLayer2Ids, r2[__rowid__])
                                    ),
                                    (r2 as record) as record => [
                                        layer1 = null,
                                        layer2 = r2,
                                        shape  = r2[geomCol2]
                                    ]
                                )
                        in
                            matchedRows & unmatchedRows

                    else if actualJoinType = "Full Outer" then
                        let
                            matchedLayer2Ids =
                                List.Distinct(
                                    List.Combine(
                                        List.Transform(
                                            layer1Matches,
                                            (match as record) as list =>
                                                Table.Column(match[matchingRows], "__rowid__")
                                        )
                                    )
                                ),
                            layer1Results =
                                List.Combine(
                                    List.Transform(
                                        layer1Matches,
                                        (match as record) as list =>
                                            let
                                                r1 = match[row1],
                                                matches = Table.ToRecords(match[matchingRows]),
                                                shapes = match[matchingShapes]
                                            in
                                                if List.Count(matches) > 0 then
                                                    List.Transform(
                                                        matches,
                                                        (r2 as record) as record =>
                                                            let
                                                                r2id = r2[__rowid__],
                                                                shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                                shapeExtras = Record.RemoveFields(shapeData, {"shape"}),
                                                                extras = Record.RemoveFields(r2, {"shape", "__rowid__"}),
                                                                actualShape = shapeData[shape]
                                                            in
                                                                Record.Combine({
                                                                    [ layer1 = r1,
                                                                    layer2 = r2,
                                                                    shape  = actualShape
                                                                    ],
                                                                    extras,
                                                                    shapeExtras
                                                                })
                                                    )
                                                else
                                                    {
                                                        [ layer1 = r1,
                                                        layer2 = null,
                                                        shape  = match[shape1]
                                                        ]
                                                    }
                                    )
                                ),
                            unmatchedLayer2 =
                                List.Transform(
                                    Table.SelectRows(
                                        table2,
                                        (r2 as record) as logical =>
                                            not List.Contains(matchedLayer2Ids, r2[__rowid__])
                                    ),
                                    (r2 as record) as record => [
                                        layer1 = null,
                                        layer2 = r2,
                                        shape  = r2[geomCol2]
                                    ]
                                )
                        in
                            layer1Results & unmatchedLayer2

                    else
                        error "Invalid join type"
                ),

                // Normalize records to ensure consistent schema
                normalizedRows = NormalizeRecords(resultRows, actualGisOperator[additionalColumns]),
                
                // Create result table with __rowid__ column
                resultTable0 = Table.FromRecords(normalizedRows),
                resultTable = Table.AddIndexColumn(resultTable0, "__rowid__", 0, 1, Int64.Type),
                baseColumns = {"__rowid__", "layer1", "layer2", "shape"},
                allColumns = Table.ColumnNames(resultTable),
                extraColumns = List.RemoveItems(allColumns, baseColumns),
                columnOrder = baseColumns & extraColumns,
                resultTableReordered = Table.ReorderColumns(resultTable, columnOrder),
                
                // Build quadtree for the result
                qtCapacity = if List.Count(resultRows) > 100 then List.Count(resultRows) / 10 else 10,
                qt = QuadTreeCreate(qtCapacity, null),
                shapesWithIds = Table.TransformRows(
                    resultTableReordered,
                    (r as record) as record =>
                        let
                            s = r[shape],
                            sid = r[__rowid__],
                            s2 = 
                                if Record.HasFields(s, "__rowid__") then 
                                    Record.TransformFields(s, {"__rowid__", each sid}) 
                                else 
                                    if sid <> null then
                                        Record.AddField(s, "__rowid__", sid)
                                    else
                                        s
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
            gisQueryOperatorType = TQuadTreeQueryOperator, //Provided for custom operators
            gisNearestN = QuadTreeOperatorNearestN,
            gisNearest = QuadTreeOperatorNearest
        ],
		gisLayerInsertRows = LayerInsertRows,
		gisLayerJoinSpatial = LayerJoinSpatial
    ]



