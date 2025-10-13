let
    // --- Load GIS Library (your code) ---
    GISLib = mgis,

    // --- Shortcuts ---
    gisShapeCreateFromWKT   = GISLib[gisShapeCreateFromWKT],
    gisLayerCreateFromTable = GISLib[gisLayerCreateFromTable],
    gisLayerJoinSpatial     = GISLib[gisLayerJoinSpatial],
    gisContains             = GISLib[gisLayerQueryOperators][gisContains],
    gisIntersects           = GISLib[gisLayerQueryOperators][gisIntersects],
    gisWithin               = GISLib[gisLayerQueryOperators][gisWithin],

    //---------------------------------------
    // 1️⃣  Zones (LayerA)  – with descriptions
    //---------------------------------------
    TableA = #table(
        {"Name", "shape", "description"},
        {
            {"ZoneA",
             gisShapeCreateFromWKT("POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"),
             "Main zone A area"},
            {"ZoneB",
             gisShapeCreateFromWKT("POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"),
             "Main zone B area"}
        }
    ),
    LayerA = gisLayerCreateFromTable(TableA, "shape"),

    //---------------------------------------
    // 2️⃣  Sub‑zones (LayerB) – with descriptions
    //---------------------------------------
    TableB = #table(
        {"SubName", "shape", "description"},
        {
            {"Sub1",
             gisShapeCreateFromWKT("POLYGON((2 2, 2 4, 4 4, 4 2, 2 2))"),
             "Inside ZoneA"},
            {"Sub2",
             gisShapeCreateFromWKT("POLYGON((9 2, 9 8, 12 8, 12 2, 9 2))"),
             "Overlaps Zones A and B"},
            {"Sub3",
             gisShapeCreateFromWKT("POLYGON((22 2, 22 8, 24 8, 24 2, 22 2))"),
             "Outside both zones"}
        }
    ),
    LayerB = gisLayerCreateFromTable(TableB, "shape"),

    //---------------------------------------
    // 3️⃣  Intersects
    //---------------------------------------
    jIntersects = gisLayerJoinSpatial(LayerB, LayerA, gisIntersects, "Inner"),
    rIntersects =
        Table.AddColumn(
            Table.ExpandRecordColumn(
                Table.ExpandRecordColumn(
                    Table.ExpandRecordColumn(jIntersects[table], "layer1",
                        {"SubName", "description"}, {"Sub", "SubDescription"}),
                    "layer2", {"Name", "description"}, {"Zone", "ZoneDescription"}),
                "shape",
                {"Kind"},
                {"ShapeKind"}
            ),
            "Relation",
            each "Intersects"
        ),

    //---------------------------------------
    // 4️⃣  Contains (acts as Sub within Zone)
    //---------------------------------------
    jContains = gisLayerJoinSpatial(LayerB, LayerA, gisContains, "Inner"),
    rContains =
        Table.AddColumn(
            Table.ExpandRecordColumn(
                Table.ExpandRecordColumn(
                    Table.ExpandRecordColumn(jContains[table], "layer1",
                        {"SubName", "description"}, {"Sub", "SubDescription"}),
                    "layer2", {"Name", "description"}, {"Zone", "ZoneDescription"}),
                "shape",
                {"Kind"},
                {"ShapeKind"}
            ),
            "Relation",
            each "Within / Contains"
        ),

    //---------------------------------------
    // 5️⃣  Within (reverse direction)
    //---------------------------------------
    jWithin = gisLayerJoinSpatial(LayerA, LayerB, gisWithin, "Inner"),
    rWithin =
        Table.AddColumn(
            Table.ExpandRecordColumn(
                Table.ExpandRecordColumn(
                    Table.ExpandRecordColumn(jWithin[table], "layer1",
                        {"Name", "description"}, {"Zone", "ZoneDescription"}),
                    "layer2", {"SubName", "description"}, {"Sub", "SubDescription"}),
                "shape",
                {"Kind"},
                {"ShapeKind"}
            ),
            "Relation",
            each "Zone within Sub (reversed)"
        ),

    //---------------------------------------
    // 6️⃣  Combine
    //---------------------------------------
    Combined = Table.Combine({rIntersects, rContains, rWithin})
in
    Combined