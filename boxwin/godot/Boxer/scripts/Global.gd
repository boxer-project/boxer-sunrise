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

func mouse_action(event: InputEventMouseButton):
    "Return the boxer mouse code for the button action.
    ie. 0 - press/MOUSE-DOWN 1 - click/MOUSE-CLICK 2 - release/MOUSE-UP 3 - double click/ MOUSE-DOUBLE-CLICK, etc"
    var action_code = 0
    if event.double_click: action_code = 3
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

func handle_mouse_input(event, row, pos, area = BoxArea.INSIDE):
    $/root/Main.handle_mouse_input(Global.mouse_action(event), row.boxer_row, pos, row.parent_box.boxer_screen_box,
        Global.input_bits(event), area)
