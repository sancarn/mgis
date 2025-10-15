let
    // --- Load GIS Library ---
    GISLib = mgis,
    githubFolder = "C:/Users/Admin/Documents/GitHub",
    
    // --- Helper function to safely run tests ---
    RunTest = (testName as text, testFunc as function, description as text, tags as list, testType as text, testing as text) as record =>
        let
            result = try testFunc(),
            passed = if result[HasError] then false else result[Value][passed],
            errorMessage = if result[HasError] then result[Error][Message] else 
                          if not passed then result[Value][errorMessage] else "",
            actual = if result[HasError] then null else result[Value][actual],
            expected = if result[HasError] then null else result[Value][expected],
            metadata = if result[HasError] then [Error = result[Error]] else result[Value][metadata]
        in
            [
                name = testName,
                description = description,
                tags = tags,
                testType = testType,
                testing = testing,
                expected = expected,
                actual = actual,
                passed = passed,
                errorMessage = errorMessage,
                metadata = metadata
            ],
    
    // --- Helper to find a row in a table by criteria ---
    FindRow = (tbl as table, criteriaRecord as record) as record =>
        let
            filtered = Table.SelectRows(tbl, (row) => 
                List.AllTrue(
                    List.Transform(
                        Record.FieldNames(criteriaRecord),
                        (fieldName) => 
                            let
                                rowValue = Record.Field(row, fieldName),
                                expectedValue = Record.Field(criteriaRecord, fieldName)
                            in
                                if expectedValue = null then rowValue = null
                                else if Value.Type(rowValue) = type record then
                                    Record.HasFields(rowValue, Record.FieldNames(expectedValue)) and
                                    List.AllTrue(List.Transform(Record.FieldNames(expectedValue), 
                                        (fn) => Record.Field(rowValue, fn) = Record.Field(expectedValue, fn)))
                                else rowValue = expectedValue
                    )
                )
            ),
            rowCount = Table.RowCount(filtered)
        in
            if rowCount = 0 then null
            else if rowCount = 1 then filtered{0}
            else filtered{0}, // Return first match if multiple

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
    Test1_1_Function = () =>
        let
            Result = Test1_Setup,
            row = FindRow(Result, [layer1 = [ID = 1]]),
            zoneName = if row <> null and row[layer2] <> null then row[layer2][Name] else null,
            expectedZone = "ZoneA",
            passed = zoneName = expectedZone
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Point ID=1 should be in ZoneA, found: " & (if zoneName = null then "null" else zoneName)
                    else "",
                expected = expectedZone,
                actual = zoneName,
                metadata = [Row = row, FullTable = Result]
            ],

    // Test 1.2: Point ID=2 at (15,5) should be in ZoneB
    Test1_2_Function = () =>
        let
            Result = Test1_Setup,
            row = FindRow(Result, [layer1 = [ID = 2]]),
            zoneName = if row <> null and row[layer2] <> null then row[layer2][Name] else null,
            expectedZone = "ZoneB",
            passed = zoneName = expectedZone
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Point ID=2 should be in ZoneB, found: " & (if zoneName = null then "null" else zoneName)
                    else "",
                expected = expectedZone,
                actual = zoneName,
                metadata = [Row = row]
            ],

    // Test 1.3: Point ID=3 at (25,5) should not be in any zone
    Test1_3_Function = () =>
        let
            Result = Test1_Setup,
            row = FindRow(Result, [layer1 = [ID = 3]]),
            zoneName = if row <> null and row[layer2] <> null then row[layer2][Name] else null,
            passed = zoneName = null
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Point ID=3 should not be in any zone, found: " & (if zoneName = null then "null" else zoneName)
                    else "",
                expected = null,
                actual = zoneName,
                metadata = [Row = row]
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
    Test2_1_Function = () =>
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
                passed = passed,
                errorMessage = if not passed then 
                    "ZoneA should match with 2 points, found: " & Text.From(rowCount)
                    else "",
                expected = expectedCount,
                actual = rowCount,
                metadata = [ZoneARows = zoneARows, FullTable = Result]
            ],

    // Test 2.2: ZoneB should contain points
    Test2_2_Function = () =>
        let
            Result = Test2_Setup,
            zoneBRows = Table.SelectRows(Result, each [layer1][Name] = "ZoneB"),
            rowCount = Table.RowCount(zoneBRows),
            // ZoneB contains point 2, so should have 1 row
            expectedCount = 1,
            passed = rowCount = expectedCount
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "ZoneB should match with 1 point, found: " & Text.From(rowCount)
                    else "",
                expected = expectedCount,
                actual = rowCount,
                metadata = [ZoneBRows = zoneBRows]
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
    Test3_1_Function = () =>
        let
            Result = Test3_Setup_Intersects,
            row = FindRow(Result, [layer1 = [SubName = "Sub1"]]),
            zoneName = if row <> null and row[layer2] <> null then row[layer2][Name] else null,
            expectedZone = "ZoneA",
            passed = zoneName = expectedZone
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Sub1 should intersect with ZoneA, found: " & (if zoneName = null then "null" else zoneName)
                    else "",
                expected = expectedZone,
                actual = zoneName,
                metadata = [Row = row, Table = Result]
            ],

    // Test 3.2: Sub2 should intersect with both ZoneA and ZoneB
    Test3_2_Function = () =>
        let
            Result = Test3_Setup_Intersects,
            sub2Rows = Table.SelectRows(Result, each [layer1][SubName] = "Sub2"),
            rowCount = Table.RowCount(sub2Rows),
            expectedCount = 2,  // Should intersect both zones
            passed = rowCount = expectedCount
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Sub2 should intersect with 2 zones, found: " & Text.From(rowCount)
                    else "",
                expected = expectedCount,
                actual = rowCount,
                metadata = [Sub2Rows = sub2Rows]
            ],

    // Test 3.3: Sub3 should not intersect with any zone (Inner join)
    Test3_3_Function = () =>
        let
            Result = Test3_Setup_Intersects,
            sub3Rows = Table.SelectRows(Result, each [layer1][SubName] = "Sub3"),
            rowCount = Table.RowCount(sub3Rows),
            expectedCount = 0,
            passed = rowCount = expectedCount
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Sub3 should not intersect with any zone, found: " & Text.From(rowCount) & " rows"
                    else "",
                expected = expectedCount,
                actual = rowCount,
                metadata = [Sub3Rows = sub3Rows]
            ],

    // Test 3.4: Sub1 should be fully contained within ZoneA
    Test3_4_Function = () =>
        let
            Result = Test3_Setup_Contains,
            row = FindRow(Result, [layer1 = [SubName = "Sub1"]]),
            zoneName = if row <> null and row[layer2] <> null then row[layer2][Name] else null,
            expectedZone = "ZoneA",
            passed = zoneName = expectedZone
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "Sub1 should be contained within ZoneA, found: " & (if zoneName = null then "null" else zoneName)
                    else "",
                expected = expectedZone,
                actual = zoneName,
                metadata = [Row = row]
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
    Test4_1_Function = () =>
        let
            Result = Test4_Setup,
            row = FindRow(Result, [layer1 = [HouseID = "H1"]]),
            nearestShop = if row <> null and row[layer2] <> null then row[layer2][ShopID] else null,
            expectedShop = "S1",
            passed = nearestShop = expectedShop
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "H1 should be nearest to S1, found: " & (if nearestShop = null then "null" else nearestShop)
                    else "",
                expected = expectedShop,
                actual = nearestShop,
                metadata = [Row = row, Table = Result]
            ],

    // Test 4.2: H2 at (4,3) should be nearest to S2 at (5,2)
    Test4_2_Function = () =>
        let
            Result = Test4_Setup,
            row = FindRow(Result, [layer1 = [HouseID = "H2"]]),
            nearestShop = if row <> null and row[layer2] <> null then row[layer2][ShopID] else null,
            expectedShop = "S2",
            passed = nearestShop = expectedShop
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "H2 should be nearest to S2, found: " & (if nearestShop = null then "null" else nearestShop)
                    else "",
                expected = expectedShop,
                actual = nearestShop,
                metadata = [Row = row]
            ],

    // Test 4.3: H3 at (9,6) should be nearest to S3 at (10,6)
    Test4_3_Function = () =>
        let
            Result = Test4_Setup,
            row = FindRow(Result, [layer1 = [HouseID = "H3"]]),
            nearestShop = if row <> null and row[layer2] <> null then row[layer2][ShopID] else null,
            expectedShop = "S3",
            passed = nearestShop = expectedShop
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "H3 should be nearest to S3, found: " & (if nearestShop = null then "null" else nearestShop)
                    else "",
                expected = expectedShop,
                actual = nearestShop,
                metadata = [Row = row]
            ],

    // Test 4.4: H4 at (14,3) should be nearest to S4 at (15,3)
    Test4_4_Function = () =>
        let
            Result = Test4_Setup,
            row = FindRow(Result, [layer1 = [HouseID = "H4"]]),
            nearestShop = if row <> null and row[layer2] <> null then row[layer2][ShopID] else null,
            expectedShop = "S4",
            passed = nearestShop = expectedShop
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "H4 should be nearest to S4, found: " & (if nearestShop = null then "null" else nearestShop)
                    else "",
                expected = expectedShop,
                actual = nearestShop,
                metadata = [Row = row]
            ],

    // Test 4.5: H5 at (18,8) should be nearest to S5 at (20,10)
    Test4_5_Function = () =>
        let
            Result = Test4_Setup,
            row = FindRow(Result, [layer1 = [HouseID = "H5"]]),
            nearestShop = if row <> null and row[layer2] <> null then row[layer2][ShopID] else null,
            expectedShop = "S5",
            passed = nearestShop = expectedShop
        in
            [
                passed = passed,
                errorMessage = if not passed then 
                    "H5 should be nearest to S5, found: " & (if nearestShop = null then "null" else nearestShop)
                    else "",
                expected = expectedShop,
                actual = nearestShop,
                metadata = [Row = row]
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
    Test5_1_Function = () =>
        let
            layer = Test5_Setup,
            hasTable = Record.HasFields(layer, "table"),
            passed = hasTable
        in
            [
                passed = passed,
                errorMessage = if not passed then "Layer missing 'table' field" else "",
                expected = true,
                actual = hasTable,
                metadata = [Layer = layer]
            ],

    // Test 5.2: Shapefile layer should have geometryColumn field
    Test5_2_Function = () =>
        let
            layer = Test5_Setup,
            hasGeometryColumn = Record.HasFields(layer, "geometryColumn"),
            passed = hasGeometryColumn
        in
            [
                passed = passed,
                errorMessage = if not passed then "Layer missing 'geometryColumn' field" else "",
                expected = true,
                actual = hasGeometryColumn,
                metadata = [Layer = layer]
            ],

    // Test 5.3: Shapefile layer should have queryLayer field
    Test5_3_Function = () =>
        let
            layer = Test5_Setup,
            hasQueryLayer = Record.HasFields(layer, "queryLayer"),
            passed = hasQueryLayer
        in
            [
                passed = passed,
                errorMessage = if not passed then "Layer missing 'queryLayer' field" else "",
                expected = true,
                actual = hasQueryLayer,
                metadata = [Layer = layer]
            ],

    // Test 5.4: Shapefile table should have shape column
    Test5_4_Function = () =>
        let
            layer = Test5_Setup,
            table = layer[table],
            hasShapeColumn = Table.HasColumns(table, {"shape"}),
            passed = hasShapeColumn
        in
            [
                passed = passed,
                errorMessage = if not passed then "Table missing 'shape' column" else "",
                expected = true,
                actual = hasShapeColumn,
                metadata = [TableColumns = Table.ColumnNames(table)]
            ],

    // Test 5.5: Shapefile should have valid shape objects
    Test5_5_Function = () =>
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
                passed = passed,
                errorMessage = if not passed then 
                    if rowCount = 0 then "No rows in shapefile"
                    else "Shape object missing required fields"
                    else "",
                expected = true,
                actual = shapeIsValid,
                metadata = [
                    RowCount = rowCount,
                    FirstShapeKind = if firstShape <> null then firstShape[Kind] else "No shapes"
                ]
            ],

    // Test 5.6: Shapefile projection should be loaded
    Test5_6_Function = () =>
        let
            layer = Test5_Setup,
            hasProjection = Record.HasFields(layer, "TProjection"),
            projectionLoaded = hasProjection and layer[TProjection] <> null,
            passed = projectionLoaded
        in
            [
                passed = passed,
                errorMessage = if not passed then "Projection not loaded from .prj file" else "",
                expected = true,
                actual = projectionLoaded,
                metadata = [
                    ProjectionData = if projectionLoaded then layer[TProjection][Data] else "No projection file"
                ]
            ],

    //===========================================
    // Execute All Tests
    //===========================================
    TestResults = {
        // Test 1: Contains - Points in Polygons
        RunTest(
            "Test1.1:Contains_PointInZoneA",
            Test1_1_Function,
            "Point ID=1 at (5,5) should be contained in ZoneA",
            {"Spatial Join", "Contains", "Points", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)"
        ),
        RunTest(
            "Test1.2:Contains_PointInZoneB",
            Test1_2_Function,
            "Point ID=2 at (15,5) should be contained in ZoneB",
            {"Spatial Join", "Contains", "Points", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)"
        ),
        RunTest(
            "Test1.3:Contains_PointOutsideZones",
            Test1_3_Function,
            "Point ID=3 at (25,5) should not be in any zone (null)",
            {"Spatial Join", "Contains", "Points", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)"
        ),

        // Test 2: Within - Reverse Contains
        RunTest(
            "Test2.1:Within_ZoneAContainsPoints",
            Test2_1_Function,
            "ZoneA should contain 2 points (ID=1 and ID=4)",
            {"Spatial Join", "Within", "Points", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisWithin...)"
        ),
        RunTest(
            "Test2.2:Within_ZoneBContainsPoints",
            Test2_2_Function,
            "ZoneB should contain 1 point (ID=2)",
            {"Spatial Join", "Within", "Points", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisWithin...)"
        ),

        // Test 3: Intersects, Contains, Within with Polygons
        RunTest(
            "Test3.1:Intersects_Sub1WithZoneA",
            Test3_1_Function,
            "Sub1 polygon should intersect with ZoneA",
            {"Spatial Join", "Intersects", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisIntersects...)"
        ),
        RunTest(
            "Test3.2:Intersects_Sub2WithBothZones",
            Test3_2_Function,
            "Sub2 polygon should intersect with both ZoneA and ZoneB (2 rows)",
            {"Spatial Join", "Intersects", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisIntersects...)"
        ),
        RunTest(
            "Test3.3:Intersects_Sub3WithNoZones",
            Test3_3_Function,
            "Sub3 polygon should not intersect with any zone (Inner join = 0 rows)",
            {"Spatial Join", "Intersects", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisIntersects...)"
        ),
        RunTest(
            "Test3.4:Contains_Sub1WithinZoneA",
            Test3_4_Function,
            "Sub1 polygon should be fully contained within ZoneA",
            {"Spatial Join", "Contains", "Polygons"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisContains...)"
        ),

        // Test 4: Nearest Neighbor
        RunTest(
            "Test4.1:Nearest_H1ToS1",
            Test4_1_Function,
            "House H1 at (1,1) should be nearest to Shop S1 at (0,0)",
            {"Spatial Join", "Nearest", "Points", "Distance"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)"
        ),
        RunTest(
            "Test4.2:Nearest_H2ToS2",
            Test4_2_Function,
            "House H2 at (4,3) should be nearest to Shop S2 at (5,2)",
            {"Spatial Join", "Nearest", "Points", "Distance"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)"
        ),
        RunTest(
            "Test4.3:Nearest_H3ToS3",
            Test4_3_Function,
            "House H3 at (9,6) should be nearest to Shop S3 at (10,6)",
            {"Spatial Join", "Nearest", "Points", "Distance"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)"
        ),
        RunTest(
            "Test4.4:Nearest_H4ToS4",
            Test4_4_Function,
            "House H4 at (14,3) should be nearest to Shop S4 at (15,3)",
            {"Spatial Join", "Nearest", "Points", "Distance"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)"
        ),
        RunTest(
            "Test4.5:Nearest_H5ToS5",
            Test4_5_Function,
            "House H5 at (18,8) should be nearest to Shop S5 at (20,10)",
            {"Spatial Join", "Nearest", "Points", "Distance"},
            "Unit",
            "gisLayerCreateFromTableWithWKT, gisLayerJoinSpatial(...gisNearest...)"
        ),

        // Test 5: Shapefile Loading
        RunTest(
            "Test5.1:Shapefile_HasTableField",
            Test5_1_Function,
            "Shapefile layer should have 'table' field",
            {"Load", "Shapefile"},
            "Unit",
            "gisLayerCreateFromShapefile"
        ),
        RunTest(
            "Test5.2:Shapefile_HasGeometryColumnField",
            Test5_2_Function,
            "Shapefile layer should have 'geometryColumn' field",
            {"Load", "Shapefile"},
            "Unit",
            "gisLayerCreateFromShapefile"
        ),
        RunTest(
            "Test5.3:Shapefile_HasQueryLayerField",
            Test5_3_Function,
            "Shapefile layer should have 'queryLayer' field",
            {"Load", "Shapefile"},
            "Unit",
            "gisLayerCreateFromShapefile"
        ),
        RunTest(
            "Test5.4:Shapefile_HasShapeColumn",
            Test5_4_Function,
            "Shapefile table should have 'shape' column",
            {"Load", "Shapefile"},
            "Unit",
            "gisLayerCreateFromShapefile"
        ),
        RunTest(
            "Test5.5:Shapefile_ValidShapeObjects",
            Test5_5_Function,
            "Shapefile should contain valid shape objects with required fields",
            {"Load", "Shapefile"},
            "Unit",
            "gisLayerCreateFromShapefile"
        ),
        RunTest(
            "Test5.6:Shapefile_ProjectionLoaded",
            Test5_6_Function,
            "Shapefile projection should be loaded from .prj file",
            {"Load", "Shapefile", "Projection"},
            "Unit",
            "gisLayerCreateFromShapefile"
        )
    },

    // Convert to table
    ResultTable = Table.FromRecords(TestResults)
in
    ResultTable

