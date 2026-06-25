extends Panel
class_name BoxHotspot

@onready var transparent_stylebox = ResourceLoader.load("res://themes/transparent_stylebox.tres")

@export var box_area: Global.BoxArea = Global.BoxArea.INSIDE

func _on_mouse_entered() -> void:
    self.remove_theme_stylebox_override("panel")

func _on_mouse_exited() -> void:
    self.add_theme_stylebox_override("panel", transparent_stylebox)

func _ready() -> void:
    self.add_theme_stylebox_override("panel", transparent_stylebox)

func get_box():
    # Return the box this hotspot belongs too
    return Global.get_parent_box(self)
