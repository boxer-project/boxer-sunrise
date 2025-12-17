extends Label

signal clicked

# A bizarre hack to keep our cursor from skipping to the front. For some reason the first time we get the position
# of this it hasn't been moved to it's correct sibling location yet.
#
# Log:
# Applying: Main:<Node#36893099383> : _on_gd_boxer_boxer_insert_cha : [Row:<HBoxContainer#51858376067>, 102, 47]
# Applying: Main:<Node#36893099383> : _on_gd_boxer_boxer_point_location : [Row:<HBoxContainer#51858376067>, 48]
# Updating the cursor in Godot: Row:<HBoxContainer#51858376067> : 48
# 48, Row:<HBoxContainer#51858376067>, 48 : (17.0, 69.0) T: f
# 48, Row:<HBoxContainer#51858376067>, 48 : (380.0, 69.0) T: f
# 48, Row:<HBoxContainer#51858376067>, 48 : (380.0, 69.0) T: f
# 48, Row:<HBoxContainer#51858376067>, 48 : (380.0, 69.0) T: f
#
# This is still not perfect, but it's better. The cursor sometimes lags a few characters behind when typing quickly.
var skip_position = true

func _on_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.is_pressed():
        var boxerref = self.get_parent().boxer_row
        clicked.emit(self)
        var pos = self.get_parent().chas.find(self)
        print("Cha clicked2: ", self, " parent: ", self.get_parent().boxer_row, " pos: ", pos)
        $/root/Main.handle_mouse_input(0, boxerref, pos + 1, 0, 0, 0)
