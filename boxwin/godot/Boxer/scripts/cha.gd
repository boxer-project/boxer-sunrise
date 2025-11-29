extends Label

signal clicked

func _on_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.is_pressed():
        var boxerref = self.get_parent().boxer_row
        clicked.emit(self)
        var pos = self.get_parent().chas.find(self)
        print("Cha clicked2: ", self, " parent: ", self.get_parent().boxer_row, " pos: ", pos)
        $/root/Main.handle_mouse_input(0, boxerref, pos + 1, 0, 0, 0)
