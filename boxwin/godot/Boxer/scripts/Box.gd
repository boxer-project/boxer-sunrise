@icon("res://media/icons/Box_scene_icon.svg")
extends BoxContainer

@onready var data_box_stylebox = ResourceLoader.load("res://themes/data_box_stylebox.tres")
@onready var doit_box_stylebox = ResourceLoader.load("res://themes/doit_box_stylebox.tres")

signal row_inserted
signal full_screened
signal flipped(box)

@export var row_scene: PackedScene

# This is the analog of a boxes fixed display style property
#@export var fixedSize = false
#func set_fixed_size()

var rows = []
var first_inferior_row
var current_row = null

# Reference to the actual Boxer box object in common lisp
var boxer_box

var skip_position = false

# defclass Box.name -> name
#   (this is actually a NameRow object in lisp)
@export var box_name = ""

# defclass Display-Style.style -> dislay-style-style
enum DisplayStyle {SUPERSHRUNK = 0, SHRUNK = 1, NORMAL = 2, FIXED = 3}

enum BoxType {DATA, DOIT, PORT}
var box_type = BoxType.DATA:
    get:
        return box_type
    set(value):
        update_box_type(value)
        box_type = value

enum FlippedBoxType {NONE = 0, GRAPHICS = 1, SPRITE = 2}
var flipped_box_type = FlippedBoxType.NONE

func update_box_type(value):
    if value == BoxType.DATA:
        $BoxInternals/OuterBorderPanel.add_theme_stylebox_override("panel", data_box_stylebox)
    elif value == BoxType.DOIT:
        $BoxInternals/OuterBorderPanel.add_theme_stylebox_override("panel", doit_box_stylebox)

func toggle_to_data():
    box_type = BoxType.DATA

func toggle_to_doit():
    box_type = BoxType.DOIT

func show_turtle_graphics(status):
    # Takes a boolean determining whether to show the turtle graphics
    %TurtleGraphics.visible = status

# Graphics and Sprite boxes still have text on their flipped side in addition to the graphics or sprite
enum BoxContents {TEXT, GRAPHICS, VIDEO}
var box_contents = BoxContents.TEXT:
    get:
        return box_contents
    set(value):
        box_contents = value
        %RowsBox.visible = false
        %GraphicsSheetBackground.visible = false
        %GraphicsSheetBitArray.visible = false
        show_turtle_graphics(false)
        %VideoPlayer.visible = false
        %PanelContainer.custom_minimum_size = Vector2(0,0)
        if value == BoxContents.TEXT:
            %RowsBox.visible = true
        elif value == BoxContents.GRAPHICS:
            %GraphicsSheetBitArray.visible = true
            %GraphicsSheetBackground.visible = true
            show_turtle_graphics(true)
            %PanelContainer.custom_minimum_size = Vector2(draw_wid, draw_hei)
        elif value == BoxContents.VIDEO:
            %VideoPlayer.visible = true

###
#
#  Vars from optional Graphics Sheet which graphics boxes have.
#
###

var draw_wid: int
var draw_hei: int
var background: Color

func set_background(red, green, blue, alpha):
    background = Color(red, green, blue, alpha)
    %GraphicsSheetBackground.color = background

func set_bit_array(width, height, arr: PackedInt32Array):
    var texture = ImageTexture.create_from_image(
        Image.create_from_data(width, height, false, Image.FORMAT_RGBA8 , arr.to_byte_array()))
    %GraphicsSheetBitArray.texture = texture

###
#
#  Vars from box.display-style
#
###


# display-style-graphics-mode?
# boolean that determines if this box is currently showing it's text side, or if it's flipped
# to graphics
var graphics_mode_p = false:
    get:
        return graphics_mode_p
    set(value):
        graphics_mode_p = value
        if graphics_mode_p:
            box_contents = BoxContents.GRAPHICS
        else:
            box_contents = BoxContents.TEXT

func set_graphics_mode_p(value: int):
    if value > 0:
        graphics_mode_p = true
    else:
        graphics_mode_p = false

var display_style = DisplayStyle.NORMAL:
    get:
        return display_style
    set(value):
        print("BLAM Set diaplay_style: ", display_style)
        update_display_style(value)
        display_style = value

func update_display_style(value):
    match value:
        DisplayStyle.FIXED:
            $BoxInternals.visible = true
            $SuperShrunkBox.visible = false
            %RowsBox.visible = true
            %ShrunkBox.visible = false
        DisplayStyle.NORMAL:
            $BoxInternals.visible = true
            $SuperShrunkBox.visible = false
            %RowsBox.visible = true
            %ShrunkBox.visible = false
        DisplayStyle.SHRUNK:
            $BoxInternals.visible = true
            $SuperShrunkBox.visible = false
            %RowsBox.visible = false
            %ShrunkBox.visible = true
        DisplayStyle.SUPERSHRUNK:
            $BoxInternals.visible = false
            $SuperShrunkBox.visible = true
        _:
            pass

func add_row() -> Node:
    var row = row_scene.instantiate()
    row.parent_box = self
    rows.append(row)
    %RowsBox.add_child(row)
    row_inserted.emit(row, 0)
    return row

func insert_row_at_row_no(row: Node, row_no: int) -> Node:
    row.parent_box = self
    rows.insert(row_no, row)
    if row.get_parent():
        var row_parent = row.get_parent()
        row_parent.remove_child(row)
    if rows_box == null:
        rows_box = %RowsBox
    rows_box.add_child.call_deferred(row)
    rows_box.move_child.call_deferred(row, row_no)
    return row

func delete_row_at_row_no(pos: int) -> void:
    var to_remove = rows.pop_at(pos)
    if to_remove:
        to_remove.queue_free()

# rows_box and name_row are here because %var lookups don't work across threads in our queues
@onready
var rows_box = %RowsBox

@onready
var name_row = %NameRow
func get_name_row():
    return name_row

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    if self.name == "World":
        current_row = add_row()
        first_inferior_row = current_row
    # Sometimes the box_type is set in lisp before it's added to the scene tree so
    # the styles don't update
    box_type = box_type
    pass

## Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta: float) -> void:
    if rows_box == null: rows_box = %RowsBox
    if name_row == null: name_row = %NameRow
    update_display_style(display_style)
    if flipped_box_type == FlippedBoxType.GRAPHICS:
        graphics_mode_p = graphics_mode_p

var moving = false
var moved = false

func _on_lower_right_corner_gui_input(event: InputEvent) -> void:
    #print("on lower right corner: ", event)
    if event is InputEventMouseButton and event.pressed:
        moving = event.pressed
    elif event is InputEventMouseButton and not event.pressed:
        moving = event.pressed
        if not moved:
            # single click means to set the box back to fixed size
            print("Back to normal3...")
            $BoxInternals.custom_minimum_size = Vector2(0,0)
            self.display_style = DisplayStyle.NORMAL
        moved = false
    elif event is InputEventMouseMotion and moving:
        #print("Relative change: ", event.relative.x, " , ", event.relative.y)
        moved = true
        $BoxInternals.custom_minimum_size = Vector2($BoxInternals.size.x + event.relative.x,
            $BoxInternals.size.y + event.relative.y)
        self.display_style = DisplayStyle.FIXED


func _on_upper_left_corner_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        if display_style == DisplayStyle.NORMAL or display_style == DisplayStyle.FIXED:
            display_style = DisplayStyle.SHRUNK
        elif display_style == DisplayStyle.SHRUNK:
            display_style = DisplayStyle.SUPERSHRUNK


func _on_shrunk_panel_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        display_style = DisplayStyle.SHRUNK


func _on_shrunk_box_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        display_style = DisplayStyle.NORMAL


func _on_upper_right_corner_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        if display_style == DisplayStyle.NORMAL or display_style == DisplayStyle.FIXED:
            #display_style = DisplayStyle.NORMAL
            print("Full screening box...")
            full_screened.emit(self)
        elif display_style == DisplayStyle.SHRUNK:
            display_style = DisplayStyle.NORMAL


func _on_lower_left_corner_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        print("Flip to graphics x: ", self.global_position.x, " y: ", self.global_position.y,
          " w: ", self.size.x, " h: ", self.size.y)
        graphics_mode_p = !graphics_mode_p


func _on_type_toggle_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        if box_type == BoxType.DATA:
            box_type = BoxType.DOIT
        elif box_type == BoxType.DOIT:
            box_type = BoxType.DATA

func push_graphics_command(opcode, arg1, arg2, arg3, arg4, arg5):
    %TurtleGraphics.append_draw_command([opcode, arg1, arg2, arg3, arg4, arg5])

func clear_box(bitmap = true, graphics_list = true):
    if graphics_list:
        %TurtleGraphics.clear_draw_commands()
    if bitmap:
        # TODO
        pass
