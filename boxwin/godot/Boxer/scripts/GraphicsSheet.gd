extends PanelContainer
class_name GraphicsSheet

# Reference to the actual boxer graphics-sheet object in common lisp
# May be null if it's the usual graphics-sheet in a box. This is needed
# for graphics boxtops.
var boxer_graphics_sheet

var background: Color
var draw_wid: int
var draw_hei: int

func set_draw_dims(wid: int, hei: int):
    draw_wid = wid
    draw_hei = hei
    self.custom_minimum_size = Vector2(wid, hei)

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
