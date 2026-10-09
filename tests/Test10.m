let
    // Exercise the file loaders added on master through the projection API.
    // TestAll supplies committed bytes to File.Contents in its isolated host.
    wgs = mgis[proj][fromEPSG][#"EPSG:4326"],
    web = mgis[proj][fromEPSG][#"EPSG:3857"],
    loadJSON = mgis[gisLayerCreateFromGeoJSONFile],
    loadShape = mgis[gisLayerCreateFromShapefile],
    jsonPath = testRepositoryRoot & "/tests/data/geojson/points.json",
    shpPath = testRepositoryRoot & "/tests/data/shape/POINT.shp",
    geographic = loadJSON(jsonPath),
    projectedInput = loadJSON(jsonPath, web),
    emptyJSON = loadJSON(testRepositoryRoot & "/tests/data/geojson/empty.json"),
    shapefile = loadShape(shpPath, wgs),
    unspecified = loadShape(shpPath),
    // Numerical reprojection is exercised through GeoJSON. The master branch's
    // shapefile double reader has a separate, pre-existing byte-order bug.
    projected = mgis[gisLayerReproject](geographic, web),
    joined = mgis[gisLayerJoinSpatial](geographic, projected,
        mgis[gisLayerQueryOperators][gisNearest], "Inner"),
    geometries = {
        [type = "Point", coordinates = {1, 2}],
        [type = "LineString", coordinates = {{1, 2}, {3, 4}}],
        [type = "Polygon", coordinates = {{{1, 2}, {3, 2}, {3, 4}, {1, 2}}}],
        [type = "MultiPoint", coordinates = {{1, 2}, {3, 4}}],
        [type = "MultiLineString", coordinates = {{{1, 2}, {3, 4}}}],
        [type = "MultiPolygon", coordinates = {{{{1, 2}, {3, 2}, {3, 4}, {1, 2}}}}],
        [type = "GeometryCollection", geometries = {[type = "Point", coordinates = {1, 2}]}]
    },
    recordParser = mgis[gisShapeCreateFromGeoJSONRecord],
    checks = [
        GeoJSONFileDefaultsToWGS84 = geographic[TProjection] = wgs
            and Table.RowCount(geographic[table]) = 3
            and geographic[table]{0}[properties][Name] = "First",
        GeoJSONFileExplicitCRS = projectedInput[TProjection] = web
            and projectedInput[table]{0}[shape][Geometry] = geographic[table]{0}[shape][Geometry],
        EmptyGeoJSONFile = emptyJSON[TProjection] = wgs
            and Table.RowCount(emptyJSON[table]) = 0
            and Table.HasColumns(emptyJSON[table], {"properties", "shape", "__rowid__"}),
        InvalidFileRejected = (try Table.RowCount(loadJSON(
            testRepositoryRoot & "/tests/data/geojson/not-a-collection.json")[table]))[HasError],
        ShapefileExplicitCRS = shapefile[TProjection] = wgs
            and Table.RowCount(shapefile[table]) = 3,
        ShapefileWKTMetadata = Text.StartsWith(shapefile[projectionWKT], "GEOGCS[")
            and unspecified[projectionWKT] = shapefile[projectionWKT],
        ShapefileWithoutAnalysisCRS = unspecified[TProjection] = null
            and (try Table.RowCount(mgis[gisLayerReproject](unspecified, web)[table]))[HasError],
        MixedFileLayerJoin = joined[TProjection] = web
            and Table.RowCount(joined[table]) = 3
            and List.AllTrue(List.Transform(joined[table][dist], each Number.Abs(_) < 0.000001)),
        TableAndIndexRowIds = List.AllTrue(List.Transform(Table.ToRecords(shapefile[table]),
            each [shape][__rowid__] = [__rowid__])),
        GeoJSONRecordAPI = List.AllTrue(List.Transform(geometries,
            each recordParser(_) = mgis[gisShapeCreateFromGeoJSON](_)))
    ]
in
    if List.AllTrue(Record.FieldValues(checks)) then checks
    else error Error.Record("TestFailure", "File loader projection integration failed.", checks)
