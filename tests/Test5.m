let
    // Test for LayerCreateFromShapefile function
    // This test loads a shapefile and verifies the layer is created correctly
    ghub = "C:/Github",
    gis = mgis,
    
    // Load shapefile from tests/data/shape/POINT.shp
    shapefilePath = ghub &"/mgis/tests/data/shape/POINT.shp",
    layer = gis[gisLayerCreateFromShapefile](shapefilePath),
    
    // Verify layer structure
    hasTable = Record.HasFields(layer, "table"),
    hasGeometryColumn = Record.HasFields(layer, "geometryColumn"),
    hasQueryLayer = Record.HasFields(layer, "queryLayer"),
    hasProjection = Record.HasFields(layer, "TProjection"),
    
    // Verify table has data
    table = layer[table],
    rowCount = Table.RowCount(table),
    hasShapeColumn = Table.HasColumns(table, {"shape"}),
    
    // Verify projection was loaded (if .prj file exists)
    projectionLoaded = layer[TProjection] <> null,
    
    // Get first shape to verify geometry is correct
    firstRow = Table.FirstN(table, 1),
    firstShape = if rowCount > 0 then firstRow{0}[shape] else null,
    shapeIsValid = if firstShape <> null then 
        Record.HasFields(firstShape, {"__TShapeIdentifier__", "Kind", "Geometry", "Envelope"})
        else false,
    
    // Result summary
    result = [
        TestName = "LayerCreateFromShapefile",
        Passed = hasTable and hasGeometryColumn and hasQueryLayer and hasProjection and hasShapeColumn and shapeIsValid,
        Details = [
            LayerStructure = [
                HasTable = hasTable,
                HasGeometryColumn = hasGeometryColumn,
                HasQueryLayer = hasQueryLayer,
                HasProjection = hasProjection
            ],
            TableInfo = [
                RowCount = rowCount,
                HasShapeColumn = hasShapeColumn
            ],
            ProjectionInfo = [
                ProjectionLoaded = projectionLoaded,
                ProjectionData = if projectionLoaded then layer[TProjection][Data] else "No projection file"
            ],
            GeometryInfo = [
                FirstShapeValid = shapeIsValid,
                FirstShapeKind = if firstShape <> null then firstShape[Kind] else "No shapes"
            ]
        ],
        Layer = layer
    ]
in
    result

