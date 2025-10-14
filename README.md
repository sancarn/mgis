# M-GIS 🗺️

A powerful GIS (Geographic Information System) library for the PowerQuery M-language ecosystem, bringing spatial analysis capabilities to Power BI, Excel, and other M-language environments.

## Features

- **Spatial Data Types**: Support for all standard WKT geometry types (Point, LineString, Polygon, MultiPoint, MultiLineString, MultiPolygon, GeometryCollection)
- **Spatial Indexing**: Built-in QuadTree implementation for efficient spatial queries
- **Spatial Queries**: `Intersects`, `Contains`, `Within`, and `Nearest Neighbor` operations
- **Spatial Joins**: Perform spatial joins between layers with support for `Inner`, `Left Outer`, `Right Outer`, and `Full Outer` joins
- **Well-Known Text (WKT)**: Easy geometry creation from WKT format
- **Layer Management**: Create and query spatial layers with automatic spatial indexing

## Installation

1. Download or copy the `mgis.m` file
2. In Power BI/Excel, go to **Get Data** → **Blank Query**
3. Open the Advanced Editor and paste the contents of `mgis.m`
4. Name the query `mgis`
5. Reference it in your queries using `mgis`

## Quick Start

```powerquery
let   
    // Create a table with WKT geometry
    MyTable = #table(
        {"Name", "shape"},
        {
            {"Location A", "POINT(5 5)"},
            {"Location B", "POINT(10 10)"}
        }
    ),
    
    // Create a spatial layer
    MyLayer = mgis[gisLayerCreateFromTableWithWKT](MyTable, "shape")
in
    MyLayer[table]
```

## API Reference

### Shape Creation

#### `gisShapeCreateFromWKT(wkt as text)`
Creates a shape from Well-Known Text representation.

```powerquery
shape = GISLib[gisShapeCreateFromWKT]("POINT(5 10)")
```

### Layer Creation

#### `gisLayerCreateBlank(geometryColumn as nullable text, capacity as nullable number)`
Creates a blank layer with no data.

#### `gisLayerCreateFromTable(table as table, geometryColumn as text)`
Creates a layer from a table containing TShape objects.

#### `gisLayerCreateFromTableWithWKT(table as table, wktColumn as text)`
Creates a layer from a table with WKT text in the specified column.

```powerquery
Layer = GISLib[gisLayerCreateFromTableWithWKT](MyTable, "shape")
```

### Spatial Query Operators

Access operators via `GISLib[gisLayerQueryOperators]`:

- **`gisIntersects`** - Returns shapes whose envelopes intersect the query shape
- **`gisContains`** - Returns shapes whose envelopes contain the query shape
- **`gisWithin`** - Returns shapes whose envelopes are within the query shape
- **`gisNearest`** - Returns the single nearest shape
- **`gisNearestN(k as number)`** - Returns k nearest shapes

### Layer Operations

#### `gisLayerQuerySpatial(layer, shape, operator)`
Queries a layer using a spatial operator.

#### `gisLayerQueryRelational(layer, function)`
Filters a layer using an attribute-based function (like `Table.SelectRows`).

#### `gisLayerJoinSpatial(layer1, layer2, operator, joinType)`
Performs a spatial join between two layers.

**Parameters:**
- `layer1`, `layer2`: The layers to join
- `operator`: Spatial operator (e.g., `gisIntersects`, `gisContains`)
- `joinType`: `"Inner"`, `"Left Outer"`, `"Right Outer"`, or `"Full Outer"`

**Returns:** A layer with columns:
- `__rowid__`: Unique row identifier
- `layer1`: Record from the first layer
- `layer2`: Record from the second layer
- `shape`: The geometry from the matched row
- Additional columns from the operator (e.g., `dist` for nearest neighbor queries)

#### `gisLayerInsertRows(layer, rows as list)`
Inserts rows into a layer and updates the spatial index.

## Examples

### Example 1: Point-in-Polygon Query

Find which points fall within which zones using a spatial join with the `gisContains` operator.

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisContains = mgis[gisLayerQueryOperators][gisContains],

    // Create polygon zones
    Zones = #table(
        {"Name", "shape"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
        }
    ),
    ZonesLayer = gisLayerCreateFromTableWithWKT(Zones, "shape"),

    // Create points
    Points = #table(
        {"ID", "shape", "description"},
        {
            {1, "POINT(5 5)", "inside ZoneA"},
            {2, "POINT(15 5)", "inside ZoneB"},
            {3, "POINT(25 5)", "outside both"}
        }
    ),
    PointsLayer = gisLayerCreateFromTableWithWKT(Points, "shape"),

    // Spatial join: which points are within which zones
    Joined = gisLayerJoinSpatial(PointsLayer, ZonesLayer, gisContains, "Left Outer")[table]
in
    Joined
```

**Result:** A table showing each point and the zone it falls within (if any).

### Example 2: Polygon Intersection Analysis

Analyze how sub-zones relate to main zones using multiple spatial operators.

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisIntersects = mgis[gisLayerQueryOperators][gisIntersects],
    gisContains = mgis[gisLayerQueryOperators][gisContains],
    gisWithin = mgis[gisLayerQueryOperators][gisWithin],

    // Main zones
    Zones = #table(
        {"Name", "shape", "description"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))", "Main zone A"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))", "Main zone B"}
        }
    ),
    ZonesLayer = gisLayerCreateFromTableWithWKT(Zones, "shape"),

    // Sub-zones
    SubZones = #table(
        {"SubName", "shape", "description"},
        {
            {"Sub1", "POLYGON((2 2, 2 4, 4 4, 4 2, 2 2))", "Inside ZoneA"},
            {"Sub2", "POLYGON((9 2, 9 8, 12 8, 12 2, 9 2))", "Overlaps both"},
            {"Sub3", "POLYGON((22 2, 22 8, 24 8, 24 2, 22 2))", "Outside both"}
        }
    ),
    SubZonesLayer = gisLayerCreateFromTableWithWKT(SubZones, "shape"),

    // Find intersections
    Intersections = gisLayerJoinSpatial(SubZonesLayer, ZonesLayer, gisIntersects, "Inner"),
    
    // Find containment (sub-zones fully within zones)
    Contains = gisLayerJoinSpatial(SubZonesLayer, ZonesLayer, gisContains, "Inner")
in
    Intersections[table]
```

**Result:** Analysis of how sub-zones spatially relate to main zones.

### Example 3: Nearest Neighbor Analysis

Find the nearest shop to each house using the `gisNearest` operator.

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisNearest = mgis[gisLayerQueryOperators][gisNearest],

    // Houses layer
    Houses = #table(
        {"HouseID", "shape"},
        {
            {"H1", "POINT(1 1)"},
            {"H2", "POINT(4 3)"},
            {"H3", "POINT(9 6)"},
            {"H4", "POINT(14 3)"},
            {"H5", "POINT(18 8)"}
        }
    ),
    HousesLayer = gisLayerCreateFromTableWithWKT(Houses, "shape"),

    // Shops layer
    Shops = #table(
        {"ShopID", "shape"},
        {
            {"S1", "POINT(0 0)"},
            {"S2", "POINT(5 2)"},
            {"S3", "POINT(10 6)"},
            {"S4", "POINT(15 3)"},
            {"S5", "POINT(20 10)"}
        }
    ),
    ShopsLayer = gisLayerCreateFromTableWithWKT(Shops, "shape"),

    // Find nearest shop to each house
    Joined = gisLayerJoinSpatial(HousesLayer, ShopsLayer, gisNearest, "Left Outer"),

    // Extract readable results
    Result = Table.ExpandRecordColumn(
        Table.ExpandRecordColumn(Joined[table], "layer1", {"HouseID"}, {"HouseID"}),
        "layer2",
        {"ShopID"},
        {"NearestShopID"}
    )
in
    Result
```

**Result:** A table showing each house and its nearest shop, including the distance.

### Example 4: Within Operator

Find zones that are within sub-zones (reverse containment).

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisWithin = mgis[gisLayerQueryOperators][gisWithin],

    // Create layers (as in Example 1)
    Zones = #table(
        {"Name", "shape"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
        }
    ),
    ZonesLayer = gisLayerCreateFromTableWithWKT(Zones, "shape"),

    Points = #table(
        {"ID", "shape"},
        {
            {1, "POINT(5 5)"},
            {2, "POINT(15 5)"},
            {3, "POINT(7 4)"}
        }
    ),
    PointsLayer = gisLayerCreateFromTableWithWKT(Points, "shape"),

    // Find zones within point envelopes (reverse relationship)
    Joined = gisLayerJoinSpatial(ZonesLayer, PointsLayer, gisWithin, "Left Outer")[table]
in
    Joined
```

## Join Types

The library supports four join types:

- **`"Inner"`**: Returns only matching rows from both layers
- **`"Left Outer"`**: Returns all rows from the first layer, with matches from the second
- **`"Right Outer"`**: Returns all rows from the second layer, with matches from the first
- **`"Full Outer"`**: Returns all rows from both layers, matching where possible

## Important Notes

1. **Envelope-based queries**: All spatial operators (`gisIntersects`, `gisContains`, `gisWithin`) currently perform envelope-based tests, not true geometric operations. This means they test bounding boxes, not the actual geometry shapes.

2. **Performance**: The library uses a QuadTree spatial index for efficient querying. For best performance with large datasets, ensure appropriate capacity settings when creating layers.

3. **Row IDs**: The library automatically manages `__rowid__` columns for internal tracking. You don't need to create these manually.

4. **WKT Format**: Geometries should be in standard WKT format:
   - `POINT(x y)`
   - `LINESTRING(x1 y1, x2 y2, ...)`
   - `POLYGON((x1 y1, x2 y2, ..., x1 y1))` (note: first and last points should match)
   - And all multi-geometries and geometry collections

## Use Cases

- **Retail Analysis**: Find stores within delivery zones, nearest competitor locations
- **Demographics**: Analyze population points within administrative boundaries
- **Logistics**: Route optimization, service area analysis
- **Environmental**: Habitat analysis, pollution zone mapping
- **Real Estate**: Property location analysis, market area definitions

## Contributing

Contributions are welcome! This library is designed to bring enterprise-grade GIS capabilities to the M-language ecosystem.

## Author

**Sancarn** - [GitHub](https://github.com/sancarn/mgis)

## License

This project is open source and available for use in both personal and commercial projects.
