extends Panel

@onready var transparent_stylebox = ResourceLoader.load("res://themes/transparent_stylebox.tres")

func _on_mouse_entered() -> void:
    self.remove_theme_stylebox_override("panel") 


func _on_mouse_exited() -> void:
    self.add_theme_stylebox_override("panel", transparent_stylebox)
    

func _ready() -> void:
    self.add_theme_stylebox_override("panel", transparent_stylebox)
