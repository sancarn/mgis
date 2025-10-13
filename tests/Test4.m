let
    // --- Load GIS Library (your mgis function record) ---
    GISLib = mgis,

    // --- Shortcuts ---
    gisLayerCreateFromTableWithWKT = GISLib[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial     = GISLib[gisLayerJoinSpatial],
    gisNearest             = GISLib[gisLayerQueryOperators][gisNearest],

    //-------------------------------------
    // 🏠  Layer A: Houses (points)
    //-------------------------------------
    HousesTable =
        #table(
            {"HouseID", "shape"},
            {
                {"H1", "POINT(1 1)"},
                {"H2", "POINT(4 3)"},
                {"H3", "POINT(9 6)"},
                {"H4", "POINT(14 3)"},
                {"H5", "POINT(18 8)"}
            }
        ),
    HousesLayer = gisLayerCreateFromTableWithWKT(HousesTable, "shape"),

    //-------------------------------------
    // 🏪  Layer B: Shops (points)
    //-------------------------------------
    ShopsTable =
        #table(
            {"ShopID", "shape"},
            {
                {"S1", "POINT(0 0)"},
                {"S2", "POINT(5 2)"},
                {"S3", "POINT(10 6)"},
                {"S4", "POINT(15 3)"},
                {"S5", "POINT(20 10)"}
            }
        ),
    ShopsLayer = gisLayerCreateFromTableWithWKT(ShopsTable, "shape"),

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