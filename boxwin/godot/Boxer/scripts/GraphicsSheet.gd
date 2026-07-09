extends PanelContainer
class_name GraphicsSheet

var background: Color

func set_background(red, green, blue, alpha):
    background = Color(red, green, blue, alpha)
    %GraphicsSheetBackground.color = background

func set_bit_array(width, height, arr: PackedInt32Array):
    var texture = ImageTexture.create_from_image(
        Image.create_from_data(width, height, false, Image.FORMAT_RGBA8 , arr.to_byte_array()))
    %GraphicsSheetBitArray.texture = texture

func push_graphics_command(opcode, arg1, arg2, arg3, arg4, arg5):
    %TurtleGraphics.append_draw_command([opcode, arg1, arg2, arg3, arg4, arg5])

func clear_box(bitmap = true, graphics_list = true):
    if graphics_list:
        %TurtleGraphics.clear_draw_commands()
    if bitmap:
        # TODO
        pass

func add_turtle(turtle):
    %TurtleGraphics.add_child(turtle)
