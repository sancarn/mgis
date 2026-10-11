// Evaluated by GenerateVoronoiExample.ps1 in the Excel Mashup engine.
// mgis, voronoiResolution and voronoiInputGeoJSON are supplied by that host. All spatial operations,
// including the transformation to GeoJSON's WGS84 coordinates, run in M.
let
    web=mgis[proj][fromEPSG][#"EPSG:3857"],
    wgs=mgis[proj][fromEPSG][#"EPSG:4326"],
    defaultSites=mgis[gisLayerCreateFromTableWithWKT](#table({"site_id","name","color","shape"},{
        {"station","Station (point)","#2563eb","POINT(-14250 6711600)"},
        {"road","Road (polyline)","#b45309","LINESTRING(-14350 6711400,-13850 6711400)"},
        {"depot","Depot (polygon)","#0f766e","POLYGON((-14000 6711580,-13900 6711580,-13900 6711700,-14000 6711700,-14000 6711580))"}
    }),"shape",web),
    colors={"#2563eb","#b45309","#0f766e"},
    input=if voronoiInputGeoJSON=null then null else
        if voronoiInputGeoJSON[type]<>"FeatureCollection" or List.IsEmpty(voronoiInputGeoJSON[features]) then
            error "Input GeoJSON must be a nonempty FeatureCollection." else voronoiInputGeoJSON[features],
    inputTable=if input=null then null else Table.FromRecords(List.Transform(List.Positions(input),(i)=>let f=input{i},
        props=f[properties] ?? [] in [site_id="site-" & Text.From(i+1),
            name=Record.FieldOrDefault(props,"name",f[geometry][type] & " " & Text.From(i+1)),
            color=colors{Number.Mod(i,List.Count(colors))},properties=props,shape=f[geometry]])),
    sites=if input=null then defaultSites else mgis[gisLayerReproject](
        mgis[gisLayerCreateFromTableWithGeoJSON](inputTable,"shape",wgs),web),
    extent=mgis[gisLayerGetExtent](sites),
    bounds=[MinX=extent[MinX]-100,MinY=extent[MinY]-100,MaxX=extent[MaxX]+100,MaxY=extent[MaxY]+100],
    cells=mgis[gisLayerCreateVoronoi](sites,bounds,[resolution=voronoiResolution]),
    geographicSites=mgis[gisLayerReproject](sites,wgs),
    geographicCells=mgis[gisLayerReproject](cells,wgs),
    coordinate=(p)=>{p[X],p[Y]},
    geometry=(g)=>let
        kind=g[Kind],
        names=[POINT="Point",LINESTRING="LineString",POLYGON="Polygon",MULTIPOINT="MultiPoint",MULTILINESTRING="MultiLineString",MULTIPOLYGON="MultiPolygon",GEOMETRYCOLLECTION="GeometryCollection"],
        coordinates=if kind="POINT" then coordinate(g)
            else if kind="LINESTRING" then List.Transform(g[Points],coordinate)
            else if kind="POLYGON" then List.Transform(g[Rings],each List.Transform([Points],coordinate))
            else List.Transform(g[Components],each @geometry(_)[coordinates])
        in if kind="GEOMETRYCOLLECTION" then [type="GeometryCollection",geometries=List.Transform(g[Components],each @geometry(_))]
            else [type=Record.Field(names,kind),coordinates=coordinates],
    feature=(row,role)=>[type="Feature",id=role & "-" & row[site_id],
        properties=Record.Combine({Record.FieldOrDefault(row,"properties",[]),[site_id=row[site_id],name=row[name],role=role,resolution=voronoiResolution,
            stroke=row[color],#"stroke-width"=if role="site" then 3 else 1.5,
            #"stroke-opacity"=1,fill=row[color],#"fill-opacity"=if role="site" then 0.45 else 0.16,
            #"marker-color"=row[color],#"marker-size"="medium"]}),
        geometry=geometry(row[shape][Geometry])],
    features=List.Buffer(List.Transform(Table.ToRecords(geographicCells[table]),each feature(_,"cell")) &
        List.Transform(Table.ToRecords(geographicSites[table]),each feature(_,"site"))),
    geographicExtent=mgis[gisLayerGetExtent](geographicCells),
    result=[type="FeatureCollection",name="M-GIS generalized Voronoi example",
        description="Source sites with their Voronoi cells; computed by mgis in the Mashup engine.",
        analysisCRS="EPSG:3857",resolution=voronoiResolution,
        bbox={geographicExtent[MinX],geographicExtent[MinY],geographicExtent[MaxX],geographicExtent[MaxY]},
        features=features]
in
    if Table.RowCount(cells[table])<>Table.RowCount(sites[table]) then error "Expected one Voronoi cell for each source site."
    else if mgis[gisLayerGetExtent](cells)<>bounds then error "Voronoi cells do not cover the supplied extent."
    else if not List.AllTrue(List.Transform(features,each mgis[gisShapeCreateFromGeoJSONRecord]([geometry])[Kind]<>null)) then
        error "Generated GeoJSON geometries could not be parsed by mgis."
    else result
