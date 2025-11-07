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
