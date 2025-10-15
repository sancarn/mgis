let
    // --- Load GIS Library ---
    GISLib = mgis,
    githubFolder = "C:/Users/Admin/Documents/GitHub",
    
    // --- Helper to check if table has at least one row matching a condition ---
    HasRowWhere = (tbl as table, condition as function) as logical =>
        let
            filtered = try Table.SelectRows(tbl, condition) otherwise #table({}, {}),
            hasMatch = Table.RowCount(filtered) > 0
        in
            hasMatch,

    // --- Shortcuts for convenience ---
    gisLayerCreateFromTableWithWKT = GISLib[gisLayerCreateFromTableWithWKT],
    gisLayerCreateFromShapefile = GISLib[gisLayerCreateFromShapefile],
    gisLayerJoinSpatial = GISLib[gisLayerJoinSpatial],
    gisContains = GISLib[gisLayerQueryOperators][gisContains],
    gisIntersects = GISLib[gisLayerQueryOperators][gisIntersects],
    gisWithin = GISLib[gisLayerQueryOperators][gisWithin],
    gisNearest = GISLib[gisLayerQueryOperators][gisNearest],

    //===========================================
    // TEST DATA SETUP: Spatial Join - Contains (Points in Polygons)
    //===========================================
    Test1_Setup = 
        let
            TableA = #table(
                {"Name", "shape"},
                {
                    {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
                    {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
                }
            ),
            LayerA = gisLayerCreateFromTableWithWKT(TableA, "shape"),
            
            TableB = #table(
                {"ID", "shape", "description"},
                {
                    {1, "POINT(5 5)", "inside ZoneA"},
                    {2, "POINT(15 5)", "inside ZoneB"},
                    {3, "POINT(25 5)", "outside both"}
                }
            ),
            LayerB = gisLayerCreateFromTableWithWKT(TableB, "shape"),
            
            Joined = gisLayerJoinSpatial(LayerB, LayerA, gisContains, "Left Outer"),
            Result = Joined[table]
        in
            Result,

    // Test 1.1: Point ID=1 at (5,5) should be in ZoneA
    Test1_1 = 
        let
            Result = Test1_Setup,
            passed = HasRowWhere(Result, each [layer1][ID] = 1 and [layer2] <> null and (try [layer2][Name] otherwise null) = "ZoneA")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)",
                description = "Point ID=1 at (5,5) should be contained in ZoneA",
                tags = {"Spatial Join", "Contains", "Points", "Polygons"},
                passed = passed
            ],

    // Test 1.2: Point ID=2 at (15,5) should be in ZoneB
    Test1_2 = 
        let
            Result = Test1_Setup,
            passed = HasRowWhere(Result, each [layer1][ID] = 2 and [layer2] <> null and (try [layer2][Name] otherwise null) = "ZoneB")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)",
                description = "Point ID=2 at (15,5) should be contained in ZoneB",
                tags = {"Spatial Join", "Contains", "Points", "Polygons"},
                passed = passed
            ],

    // Test 1.3: Point ID=3 at (25,5) should not be in any zone
    Test1_3 = 
        let
            Result = Test1_Setup,
            passed = HasRowWhere(Result, each [layer1][ID] = 3 and [layer2] = null)
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)",
                description = "Point ID=3 at (25,5) should not be in any zone (null)",
                tags = {"Spatial Join", "Contains", "Points", "Polygons"},
                passed = passed
            ],

    //===========================================
    // TEST DATA SETUP: Spatial Join - Within (Reverse Contains)
    //===========================================
    Test2_Setup = 
        let
            TableA = #table(
                {"Name", "shape"},
                {
                    {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
                    {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
                }
            ),
            LayerA = gisLayerCreateFromTableWithWKT(TableA, "shape"),
            
            TableB = #table(
                {"ID", "shape", "description"},
                {
                    {1, "POINT(5 5)", "inside ZoneA"},
                    {2, "POINT(15 5)", "inside ZoneB"},
                    {3, "POINT(25 5)", "outside both"},
                    {4, "POINT(7 4)", "also inside ZoneA"}
                }
            ),
            LayerB = gisLayerCreateFromTableWithWKT(TableB, "shape"),
            
            Joined = gisLayerJoinSpatial(LayerA, LayerB, gisWithin, "Left Outer"),
            Result = Joined[table]
        in
            Result,

    // Test 2.1: ZoneA should contain points (test returns zones that are within points)
    Test2_1 = 
        let
            Result = Test2_Setup,
            // For Within operator with left outer: each zone should appear for points within it
            zoneARows = Table.SelectRows(Result, each [layer1][Name] = "ZoneA"),
            rowCount = Table.RowCount(zoneARows),
            // ZoneA contains points 1 and 4, so should have 2 rows
            expectedCount = 2,
            passed = rowCount = expectedCount
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisWithin...)",
                description = "ZoneA should contain 2 points (ID=1 and ID=4)",
                tags = {"Spatial Join", "Within", "Points", "Polygons"},
                passed = passed,
                expected = expectedCount,
                actual = rowCount
            ],

    // Test 2.2: ZoneB should contain points
    Test2_2 = 
        let
            Result = Test2_Setup,
            zoneBRows = Table.SelectRows(Result, each [layer1][Name] = "ZoneB"),
            rowCount = Table.RowCount(zoneBRows),
            // ZoneB contains point 2, so should have 1 row
            expectedCount = 1,
            passed = rowCount = expectedCount
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisWithin...)",
                description = "ZoneB should contain 1 point (ID=2)",
                tags = {"Spatial Join", "Within", "Points", "Polygons"},
                passed = passed,
                expected = expectedCount,
                actual = rowCount
            ],

    //===========================================
    // TEST DATA SETUP: Multiple Spatial Joins - Intersects, Contains, Within
    //===========================================
    Test3_Setup_Intersects = 
        let
            TableA = #table(
                {"Name", "shape", "description"},
                {
                    {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))", "Main zone A area"},
                    {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))", "Main zone B area"}
                }
            ),
            LayerA = gisLayerCreateFromTableWithWKT(TableA, "shape"),
            
            TableB = #table(
                {"SubName", "shape", "description"},
                {
                    {"Sub1", "POLYGON((2 2, 2 4, 4 4, 4 2, 2 2))", "Inside ZoneA"},
                    {"Sub2", "POLYGON((9 2, 9 8, 12 8, 12 2, 9 2))", "Overlaps Zones A and B"},
                    {"Sub3", "POLYGON((22 2, 22 8, 24 8, 24 2, 22 2))", "Outside both zones"}
                }
            ),
            LayerB = gisLayerCreateFromTableWithWKT(TableB, "shape"),
            
            jIntersects = gisLayerJoinSpatial(LayerB, LayerA, gisIntersects, "Inner"),
            Result = jIntersects[table]
        in
            Result,

    Test3_Setup_Contains = 
        let
            TableA = #table(
                {"Name", "shape", "description"},
                {
                    {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))", "Main zone A area"},
                    {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))", "Main zone B area"}
                }
            ),
            LayerA = gisLayerCreateFromTableWithWKT(TableA, "shape"),
            
            TableB = #table(
                {"SubName", "shape", "description"},
                {
                    {"Sub1", "POLYGON((2 2, 2 4, 4 4, 4 2, 2 2))", "Inside ZoneA"},
                    {"Sub2", "POLYGON((9 2, 9 8, 12 8, 12 2, 9 2))", "Overlaps Zones A and B"},
                    {"Sub3", "POLYGON((22 2, 22 8, 24 8, 24 2, 22 2))", "Outside both zones"}
                }
            ),
            LayerB = gisLayerCreateFromTableWithWKT(TableB, "shape"),
            
            jContains = gisLayerJoinSpatial(LayerB, LayerA, gisContains, "Inner"),
            Result = jContains[table]
        in
            Result,

    // Test 3.1: Sub1 should intersect with ZoneA
    Test3_1 = 
        let
            Result = Test3_Setup_Intersects,
            passed = HasRowWhere(Result, each [layer1][SubName] = "Sub1" and [layer2] <> null and (try [layer2][Name] otherwise null) = "ZoneA")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisIntersects...)",
                description = "Sub1 polygon should intersect with ZoneA",
                tags = {"Spatial Join", "Intersects", "Polygons"},
                passed = passed
            ],

    // Test 3.2: Sub2 should intersect with both ZoneA and ZoneB
    Test3_2 = 
        let
            Result = Test3_Setup_Intersects,
            sub2Rows = Table.SelectRows(Result, each [layer1][SubName] = "Sub2"),
            rowCount = Table.RowCount(sub2Rows),
            expectedCount = 2,  // Should intersect both zones
            passed = rowCount = expectedCount
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisIntersects...)",
                description = "Sub2 polygon should intersect with both ZoneA and ZoneB (2 rows)",
                tags = {"Spatial Join", "Intersects", "Polygons"},
                passed = passed,
                expected = expectedCount,
                actual = rowCount
            ],

    // Test 3.3: Sub3 should not intersect with any zone (Inner join)
    Test3_3 = 
        let
            Result = Test3_Setup_Intersects,
            sub3Rows = Table.SelectRows(Result, each [layer1][SubName] = "Sub3"),
            rowCount = Table.RowCount(sub3Rows),
            expectedCount = 0,
            passed = rowCount = expectedCount
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisIntersects...)",
                description = "Sub3 polygon should not intersect with any zone (Inner join = 0 rows)",
                tags = {"Spatial Join", "Intersects", "Polygons"},
                passed = passed,
                expected = expectedCount,
                actual = rowCount
            ],

    // Test 3.4: Sub1 should be fully contained within ZoneA
    Test3_4 = 
        let
            Result = Test3_Setup_Contains,
            passed = HasRowWhere(Result, each [layer1][SubName] = "Sub1" and [layer2] <> null and (try [layer2][Name] otherwise null) = "ZoneA")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)",
                description = "Sub1 polygon should be fully contained within ZoneA",
                tags = {"Spatial Join", "Contains", "Polygons"},
                passed = passed
            ],

    //===========================================
    // TEST DATA SETUP: Nearest Neighbor Join
    //===========================================
    Test4_Setup = 
        let
            HousesTable = #table(
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
            
            ShopsTable = #table(
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
            
            Joined = gisLayerJoinSpatial(HousesLayer, ShopsLayer, gisNearest, "Left Outer"),
            Result = Joined[table]
        in
            Result,

    // Test 4.1: H1 at (1,1) should be nearest to S1 at (0,0)
    Test4_1 = 
        let
            Result = Test4_Setup,
            passed = HasRowWhere(Result, each [layer1][HouseID] = "H1" and [layer2] <> null and (try [layer2][ShopID] otherwise null) = "S1")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)",
                description = "House H1 at (1,1) should be nearest to Shop S1 at (0,0)",
                tags = {"Spatial Join", "Nearest", "Points", "Distance"},
                passed = passed
            ],

    // Test 4.2: H2 at (4,3) should be nearest to S2 at (5,2)
    Test4_2 = 
        let
            Result = Test4_Setup,
            passed = HasRowWhere(Result, each [layer1][HouseID] = "H2" and [layer2] <> null and (try [layer2][ShopID] otherwise null) = "S2")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)",
                description = "House H2 at (4,3) should be nearest to Shop S2 at (5,2)",
                tags = {"Spatial Join", "Nearest", "Points", "Distance"},
                passed = passed
            ],

    // Test 4.3: H3 at (9,6) should be nearest to S3 at (10,6)
    Test4_3 = 
        let
            Result = Test4_Setup,
            passed = HasRowWhere(Result, each [layer1][HouseID] = "H3" and [layer2] <> null and (try [layer2][ShopID] otherwise null) = "S3")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)",
                description = "House H3 at (9,6) should be nearest to Shop S3 at (10,6)",
                tags = {"Spatial Join", "Nearest", "Points", "Distance"},
                passed = passed
            ],

    // Test 4.4: H4 at (14,3) should be nearest to S4 at (15,3)
    Test4_4 = 
        let
            Result = Test4_Setup,
            passed = HasRowWhere(Result, each [layer1][HouseID] = "H4" and [layer2] <> null and (try [layer2][ShopID] otherwise null) = "S4")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)",
                description = "House H4 at (14,3) should be nearest to Shop S4 at (15,3)",
                tags = {"Spatial Join", "Nearest", "Points", "Distance"},
                passed = passed
            ],

    // Test 4.5: H5 at (18,8) should be nearest to S5 at (20,10)
    Test4_5 = 
        let
            Result = Test4_Setup,
            passed = HasRowWhere(Result, each [layer1][HouseID] = "H5" and [layer2] <> null and (try [layer2][ShopID] otherwise null) = "S5")
        in
            [
                parent = "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)",
                description = "House H5 at (18,8) should be nearest to Shop S5 at (20,10)",
                tags = {"Spatial Join", "Nearest", "Points", "Distance"},
                passed = passed
            ],

    //===========================================
    // TEST DATA SETUP: Load Shapefile
    //===========================================
    Test5_Setup = 
        let
            shapefilePath = githubFolder & "/mgis/tests/data/shape/POINT.shp",
            layer = gisLayerCreateFromShapefile(shapefilePath)
        in
            layer,

    // Test 5.1: Shapefile layer should have table field
    Test5_1 = 
        let
            layer = Test5_Setup,
            hasTable = Record.HasFields(layer, "table"),
            passed = hasTable
        in
            [
                parent = "gisLayerCreateFromShapefile",
                description = "Shapefile layer should have 'table' field",
                tags = {"Load", "Shapefile"},
                passed = passed
            ],

    // Test 5.2: Shapefile layer should have geometryColumn field
    Test5_2 = 
        let
            layer = Test5_Setup,
            hasGeometryColumn = Record.HasFields(layer, "geometryColumn"),
            passed = hasGeometryColumn
        in
            [
                parent = "gisLayerCreateFromShapefile",
                description = "Shapefile layer should have 'geometryColumn' field",
                tags = {"Load", "Shapefile"},
                passed = passed
            ],

    // Test 5.3: Shapefile layer should have queryLayer field
    Test5_3 = 
        let
            layer = Test5_Setup,
            hasQueryLayer = Record.HasFields(layer, "queryLayer"),
            passed = hasQueryLayer
        in
            [
                parent = "gisLayerCreateFromShapefile",
                description = "Shapefile layer should have 'queryLayer' field",
                tags = {"Load", "Shapefile"},
                passed = passed
            ],

    // Test 5.4: Shapefile table should have shape column
    Test5_4 = 
        let
            layer = Test5_Setup,
            table = layer[table],
            hasShapeColumn = Table.HasColumns(table, {"shape"}),
            passed = hasShapeColumn
        in
            [
                parent = "gisLayerCreateFromShapefile",
                description = "Shapefile table should have 'shape' column",
                tags = {"Load", "Shapefile"},
                passed = passed
            ],

    // Test 5.5: Shapefile should have valid shape objects
    Test5_5 = 
        let
            layer = Test5_Setup,
            table = layer[table],
            rowCount = Table.RowCount(table),
            firstRow = if rowCount > 0 then Table.FirstN(table, 1) else #table({}, {}),
            firstShape = if rowCount > 0 then firstRow{0}[shape] else null,
            shapeIsValid = if firstShape <> null then 
                Record.HasFields(firstShape, {"__TShapeIdentifier__", "Kind", "Geometry", "Envelope"})
                else false,
            passed = shapeIsValid
        in
            [
                parent = "gisLayerCreateFromShapefile",
                description = "Shapefile should contain valid shape objects with required fields",
                tags = {"Load", "Shapefile"},
                passed = passed
            ],

    // Test 5.6: Shapefile projection should be loaded
    Test5_6 = 
        let
            layer = Test5_Setup,
            hasProjection = Record.HasFields(layer, "TProjection"),
            projectionLoaded = hasProjection and layer[TProjection] <> null,
            passed = projectionLoaded
        in
            [
                parent = "gisLayerCreateFromShapefile",
                description = "Shapefile projection should be loaded from .prj file",
                tags = {"Load", "Shapefile", "Projection"},
                passed = passed
            ],

    //===========================================
    // Execute All Tests
    //===========================================
    TestResults = {
        // Test 1: Contains - Points in Polygons
        Test1_1,
        Test1_2,
        Test1_3,

        // Test 2: Within - Reverse Contains
        Test2_1,
        Test2_2,

        // Test 3: Intersects, Contains, Within with Polygons
        Test3_1,
        Test3_2,
        Test3_3,
        Test3_4,

        // Test 4: Nearest Neighbor
        Test4_1,
        Test4_2,
        Test4_3,
        Test4_4,
        Test4_5,

        // Test 5: Shapefile Loading
        Test5_1,
        Test5_2,
        Test5_3,
        Test5_4,
        Test5_5,
        Test5_6
    },

    // Convert to table
    ResultTable = Table.FromRecords(TestResults)
in
    ResultTable