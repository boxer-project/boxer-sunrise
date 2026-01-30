extends MeshInstance3D

var flipping = false;
var rot = 0

func _process(delta: float) -> void:
    if flipping:
        self.visible = true
        if rot < PI:
            rot += delta * 2
            self.rotation.y = rot
        else:
            self.visible = false
            flipping = false
            rot = 0
    #else:
        #rot = 0
