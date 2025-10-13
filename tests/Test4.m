let
    // --- Load GIS Library (your mgis function record) ---
    GISLib = mgis,

    // --- Shortcuts ---
    gisShapeCreateFromWKT   = GISLib[gisShapeCreateFromWKT],
    gisLayerCreateFromTable = GISLib[gisLayerCreateFromTable],
    gisLayerJoinSpatial     = GISLib[gisLayerJoinSpatial],
    gisNearest             = GISLib[gisLayerQueryOperators][gisNearest],

    //-------------------------------------
    // 🏠  Layer A: Houses (points)
    //-------------------------------------
    HousesTable =
        #table(
            {"HouseID", "shape"},
            {
                {"H1", gisShapeCreateFromWKT("POINT(1 1)")},
                {"H2", gisShapeCreateFromWKT("POINT(4 3)")},
                {"H3", gisShapeCreateFromWKT("POINT(9 6)")},
                {"H4", gisShapeCreateFromWKT("POINT(14 3)")},
                {"H5", gisShapeCreateFromWKT("POINT(18 8)")}
            }
        ),
    HousesLayer = gisLayerCreateFromTable(HousesTable, "shape"),

    //-------------------------------------
    // 🏪  Layer B: Shops (points)
    //-------------------------------------
    ShopsTable =
        #table(
            {"ShopID", "shape"},
            {
                {"S1", gisShapeCreateFromWKT("POINT(0 0)")},
                {"S2", gisShapeCreateFromWKT("POINT(5 2)")},
                {"S3", gisShapeCreateFromWKT("POINT(10 6)")},
                {"S4", gisShapeCreateFromWKT("POINT(15 3)")},
                {"S5", gisShapeCreateFromWKT("POINT(20 10)")}
            }
        ),
    ShopsLayer = gisLayerCreateFromTable(ShopsTable, "shape"),

    //-------------------------------------
    // 🔍  Perform nearest‑neighbour join
    //    Reads as: For each house, find 1 nearest shop
    //-------------------------------------
    Joined = gisLayerJoinSpatial(HousesLayer, ShopsLayer, gisNearest, "Left Outer"),

    //-------------------------------------
    // 🧾  Extract readable results
    //-------------------------------------
    Expanded =
        Table.ExpandRecordColumn(
            Table.ExpandRecordColumn(Joined[table], "layer1", {"HouseID"}, {"HouseID"}),
            "layer2",
            {"ShopID"},
            {"NearestShopID"}
        )
in
    Expanded