import bpy

i=0
order = 0
for object in bpy.data.objects:
    mesh = object.data
    if not mesh.vertex_colors:
        mesh.vertex_colors.new()
    
    mesh.vertex_colors[0].name = "PivotPoint"
    color_layer = mesh.vertex_colors[0]
    print(object.name)
    
    print("Vertex Color data - Order: ", i)
    for data in color_layer.data:
         data.color = (i,i,i,1.0)
    i+=1/(len(bpy.data.objects))

    if len(mesh.uv_layers)<2:
        mesh.uv_layers.new(name='PivotPointDataXY')
    mesh.uv_layers[1].name = 'PivotPointDataXY'
    if len(mesh.uv_layers)<3:
        mesh.uv_layers.new(name='PivotPointDataZO')
    mesh.uv_layers[2].name = 'PivotPointDataZO'
    
    xy =  mesh.uv_layers[1].data
    zo =  mesh.uv_layers[2].data
    
    print("UV data - Pivot point: ",(object.location.y,object.location.z))
    for poly in mesh.polygons:
        for loop_index in range(poly.loop_start, poly.loop_start + poly.loop_total):
            xy[loop_index].uv = (object.location.x,object.location.y)
            zo[loop_index].uv = (object.location.z,order)
    order+=1
    print('\n')
