let
    extent=mgis[gisLayerGetExtent],
    wgs=mgis[proj][fromEPSG][#"EPSG:4326"],web=mgis[proj][fromEPSG][#"EPSG:3857"],
    layer=mgis[gisLayerCreateFromTableWithWKT](#table({"Name","geom"},{
        {"Polygon","POLYGON((-2 3,4 3,4 9,-2 9,-2 3))"},
        {"Line","LINESTRING(0 -5,12 1)"},
        {"Outlier","POINT(100 200)"}
    }),"geom",web),
    filtered=mgis[gisLayerQueryRelational](layer,each [Name]<>"Outlier"),
    empty=mgis[gisLayerQueryRelational](layer,each false),
    point=mgis[gisLayerCreateFromTableWithXY](#table({"X","Y"},{{3,7}}),"X","Y"),
    flat=mgis[gisLayerCreateFromTableWithXY](#table({"X","Y"},{{3,7},{9,7}}),"X","Y"),
    source=mgis[gisLayerCreateFromTableWithXY](#table({"Lon","Lat"},{{0,0},{1,1}}),"Lon","Lat",wgs),
    projected=mgis[gisLayerReproject](source,web),
    buffered=mgis[gisLayerCreateBuffer](point,2,null,[resolution=4]),
    cells=mgis[gisLayerCreateVoronoi](source,extent(projected),[analysisCRS=web]),
    checks=[
        WholeGeometryExtent=extent(layer)=[MinX=-2,MinY=-5,MaxX=100,MaxY=200],
        FilteredRowsOnly=extent(filtered)=[MinX=-2,MinY=-5,MaxX=12,MaxY=9]
            and filtered[queryLayer][root][envelope][MaxX]=100,
        Empty=extent(empty)=null and extent(mgis[gisLayerCreateBlank]("geom",10,web))=null,
        SinglePoint=extent(point)=[MinX=3,MinY=7,MaxX=3,MaxY=7],
        CollinearPoints=extent(flat)=[MinX=3,MinY=7,MaxX=9,MaxY=7],
        BufferedGeometry=extent(buffered)=[MinX=1,MinY=5,MaxX=5,MaxY=9],
        GeographicUnits=extent(source)=[MinX=0,MinY=0,MaxX=1,MaxY=1],
        ProjectedUnits=Number.Abs(extent(projected)[MaxX]-111319.49079327357)<1e-6
            and Number.Abs(extent(projected)[MaxY]-111325.1428663851)<1e-6,
        VoronoiIntegration=cells[TProjection]=web and Table.RowCount(cells[table])=2
            and Number.Abs(extent(cells)[MaxX]-extent(projected)[MaxX])<1e-6
            and Number.Abs(extent(cells)[MaxY]-extent(projected)[MaxY])<1e-6
    ],
    evaluated=Record.FromList(List.Transform(Record.FieldNames(checks),(name)=>let result=try Record.Field(checks,name)
        in if result[HasError] then result[Error][Message] else result[Value]),Record.FieldNames(checks))
in
    if List.AllTrue(List.Transform(Record.FieldValues(evaluated),each _=true)) then evaluated
    else error Error.Record("TestFailure",Text.FromBinary(Json.FromValue(evaluated)),null)
