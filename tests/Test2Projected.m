let
    // Projection companion to Test2; the original remains a null-CRS regression.
    web = mgis[proj][fromEPSG][#"EPSG:3857"],
    wgs = mgis[proj][fromEPSG][#"EPSG:4326"],
    reproject = mgis[gisLayerReproject],
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
    LayerA = gisLayerCreateFromTableWithWKT(TableA, "shape", web),

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
    LayerB = reproject(gisLayerCreateFromTableWithWKT(TableB, "shape", web), wgs),

    //---------------------------------------
    // 3️⃣ Spatial Join – use `gisWithin`
    //---------------------------------------
    // Read like: shapes of LayerA are within shapes of LayerB (reverse test of Contains)
    JoinedLayer = gisLayerJoinSpatial(LayerA, LayerB, gisWithin, "Left Outer"),
    Joined = JoinedLayer[table]
in
    if LayerA[TProjection] = web and LayerB[TProjection] = wgs
        and JoinedLayer[TProjection] = web
        and Table.TransformRows(Joined, each {[layer1][Name], [layer2][ID]})
        = {{"ZoneA", 1}, {"ZoneA", 4}, {"ZoneB", 2}}
    then Joined
    else error "Test2Projected: reversed containment join returned unexpected matches."
