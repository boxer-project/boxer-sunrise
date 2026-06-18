# Global.gd
extends Node

@export var screen_scale: float

@export var cur_font_size = 14
@export var cur_font_color = Color("black")

func _ready() -> void:
    screen_scale = DisplayServer.screen_get_scale()
    get_viewport().content_scale_factor = screen_scale

func dpi_scale(value: float):
    # Scale the size based on the hiDPI scale
    return value / screen_scale


# This will be a vector of [row,  pos, area] which will be used on press and releases to determine
# if we are on the same widget and should emit a Boxer MOUSE-CLICK
var cur_pressed = null

func get_mouse_location():
    var control = get_viewport().gui_get_hovered_control()
    if control is Cha:
        return [control.get_parent(), control.get_index()]

func mouse_action(event: InputEventMouseButton, row, pos, area):
    "Return the boxer mouse code for the button action.
    ie. 0 - press/MOUSE-DOWN 1 - click/MOUSE-CLICK 2 - release/MOUSE-UP 3 - double click/ MOUSE-DOUBLE-CLICK, etc"
    #  0 - Primary Button, usually left click
    #  1 - Third Button
    #  2 - Secondary Button, usually right click
    #  3 - Primary Double Click
    #  4 - Middle Double Click
    #  5 - Right Double Click
    #  6 - Primary Down
    #  7 - Middle Down
    #  8 - Right Down
    #  9 - Primary Up
    # 10 - Middle Up
    # 11 - Right Up
    var action_code: int
    if event.button_index == MOUSE_BUTTON_LEFT:
        action_code = 6
    elif event.button_index == MOUSE_BUTTON_RIGHT:
        action_code = 8
    elif event.button_index == MOUSE_BUTTON_MIDDLE:
        action_code = 7

    # Double Clicked
    if event.double_click:
        action_code -= 3
        cur_pressed = null
    # Mouse Press Down
    elif event.pressed:
        cur_pressed = [row, pos, area]
    # Mouse Release
    else:
        # Click
        var hover = get_mouse_location()
        if cur_pressed != null && cur_pressed[0] == hover[0] && cur_pressed[1] == hover[1]: # && cur_pressed[2] == area:
            action_code -= 6
        # Release Up
        else:
            action_code += 3
        cur_pressed = null

    return action_code

func input_bits(event: InputEventWithModifiers):
    "Calculate the boxer event bits from this event we need to send through for keyboard/mouse handling."
    var bits = 0
    if event.shift_pressed:
        bits = bits | 1
    if event.ctrl_pressed:
        bits = bits | 2
    if event.alt_pressed:
        bits = bits | 4
    if event.meta_pressed:
        bits = bits | 8
    return bits

enum BoxArea {INSIDE = 0, OUTSIDE = 1, NAME = 2, SCROLL_BAR = 3, TYPE = 4, BOTTOM_RIGHT = 5, BOTTOM_LEFT = 6,
    TOP_RIGHT = 7, TOP_LEFT = 8}

func handle_mouse_input(event: InputEventMouse, row, pos, area = BoxArea.INSIDE):
    if event is InputEventMouseButton:
        var action: int = Global.mouse_action(event, row, pos, area)

        # TODO do double clicks in Godot emit all the stuff before them?

        # If it's a click, emit both the released and the click
        if action <= 2:
            $/root/Main.handle_mouse_input(action + 9, row.boxer_row, pos, row.parent_box.boxer_screen_box,
                Global.input_bits(event), area)
            $/root/Main.handle_mouse_input(action, row.boxer_row, pos, row.parent_box.boxer_screen_box,
                Global.input_bits(event), area)
        else:
            $/root/Main.handle_mouse_input(action, row.boxer_row, pos, row.parent_box.boxer_screen_box,
                Global.input_bits(event), area)
    elif event is InputEventMouseMotion and cur_pressed != null:
        ##TODO We can't select backwards right now, because the top-most control is the highlighted text, we need
        # to get underneath it to get to the cha, box, or row
        var control = get_viewport().gui_get_hovered_control()
        # If this is a cha
        if control is Cha:
            var cha_boxer_row = control.get_parent().boxer_row
            var cha_pos = control.get_index()
            var cha_boxer_screen_box = control.get_parent().parent_box.boxer_screen_box

            print("handle mouse input motion dragging: ", control, )
            $/root/Main.handle_boxer_func("MOUSE-UPDATE-SELECTED-REGION", cha_boxer_row, cha_pos, cha_boxer_screen_box)
