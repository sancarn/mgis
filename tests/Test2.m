let
    // --- Load GIS Library (your code) ---
    GISLib = mgis,

    // --- Shortcuts for convenience ---
    gisLayerCreateFromTableWithWKT = GISLib[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = GISLib[gisLayerJoinSpatial],
    gisContains = GISLib[gisLayerQueryOperators][gisContains],
    gisIntersects = GISLib[gisLayerQueryOperators][gisIntersects],
    gisWithin = GISLib[gisLayerQueryOperators][gisWithin],

    //---------------------------------------
    // 1️⃣  Make TableA (e.g. polygons)
    //---------------------------------------
    TableA = #table(
        {"Name", "shape"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
        }
    ),
    LayerA = gisLayerCreateFromTableWithWKT(TableA, "shape"),

    //---------------------------------------
    // 2️⃣  Make TableB (e.g. points)
    //---------------------------------------
    TableB = #table(
        {"ID", "shape", "description"},
        {
            {1, "POINT(5 5)",  "inside ZoneA"},
            {2, "POINT(15 5)", "inside ZoneB"},
            {3, "POINT(25 5)", "outside both"},
            {4, "POINT(7 4)",  "also inside ZoneA"}   // ✅ New point
        }
    ),
    LayerB = gisLayerCreateFromTableWithWKT(TableB, "shape"),

    //---------------------------------------
    // 3️⃣ Spatial Join – use `gisWithin`
    //---------------------------------------
    // Read like: shapes of LayerA are within shapes of LayerB (reverse test of Contains)
    Joined = gisLayerJoinSpatial(LayerA, LayerB, gisWithin, "Left Outer")[table]
in
    Joined