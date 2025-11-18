@tool
extends Control

signal done_drawing

var default_font : Font = ThemeDB.fallback_font;

var pen_color = Color.BLACK;
var pen_width = 1.0;

#func _process(delta: float) -> void:
    #queue_redraw()

# 33   BOXER-CHANGE-PEN-WIDTH                       (NEW-WIDTH)
func change_pen_width(width):
    pen_width = width

# 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
func line_segment(x0, y0, x1, y1):
    draw_line(Vector2(x0, -y0), Vector2(x1, -y1), pen_color, pen_width)

# 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
func change_graphics_color(color):
    pen_color = color

# 39   BOXER-CENTERED-STRING                        (X Y STRING)
func centered_string(x, y, string):
    draw_string(default_font, Vector2(x, -y), string, HorizontalAlignment.HORIZONTAL_ALIGNMENT_CENTER,
        -1, ThemeDB.fallback_font_size, pen_color)

# 40   BOXER-LEFT-STRING                            (X Y STRING)
func left_string(x, y, string):
    draw_string(default_font, Vector2(x, -y), string, HorizontalAlignment.HORIZONTAL_ALIGNMENT_LEFT,
        -1, ThemeDB.fallback_font_size, pen_color)

# 41   BOXER-RIGHT-STRING                           (X Y STRING)
func right_string(x, y, string):
    draw_string(default_font, Vector2(x, -y), string, HorizontalAlignment.HORIZONTAL_ALIGNMENT_RIGHT,
        -1, ThemeDB.fallback_font_size, pen_color)

# 42   BOXER-CENTERED-RECTANGLE                     (X Y WIDTH HEIGHT)
func centered_rectangle(x, y, width, height):
    draw_rect(Rect2(x - (width/2), -y - (height/2), width, height), pen_color)

# 44   BOXER-HOLLOW-RECTANGLE                       (X Y WIDTH HEIGHT)
func hollow_rectangle(x, y, width, height):
    draw_rect(Rect2(x - (width/2), -y - (height/2), width, height), pen_color,
        false, pen_width)

# 60   BOXER-FILLED-ELLIPSE                         (X Y WIDTH HEIGHT)
func filled_ellipse(x, y, width, height):
    draw_set_transform(Vector2(), 0.0, Vector2(1.0, float(height) / width))
    draw_circle(Vector2(x, -y), width / 2.0, pen_color)
    draw_set_transform(Vector2())

# 61   BOXER-ELLIPSE                                (X Y WIDTH HEIGHT)
func ellipse(x, y, width, height):
    draw_set_transform(Vector2(), 0.0, Vector2(1.0, float(height) / width))
    draw_circle(Vector2(x, -y), width / 2.0, pen_color, false, pen_width)
    draw_set_transform(Vector2())

# 62   BOXER-FILLED-CIRCLE                          (X Y RADIUS)
func filled_circle(x, y, radius):
    draw_circle(Vector2(x, -y), radius, pen_color)

# 63   BOXER-CIRCLE                                 (X Y RADIUS)
func circle(x, y, radius):
    draw_circle(Vector2(x, -y), radius, pen_color, false, pen_width)

func draw_graphics_command(com: Array):
    var op_code = com[0]
    if op_code == 33:
        pen_width = com[1]
    elif op_code == 35:
        line_segment(com[1], com[2], com[3], com[4])
    elif op_code == 36:
        pen_color = com[1]
    # TODO These Strings aren't all justified properly yet
    elif op_code == 39:
        centered_string(com[1], com[2], com[3])
    elif op_code == 40:
        left_string(com[1], com[2], com[3])
    elif op_code == 41:
        right_string(com[1], com[2], com[3])
    elif op_code == 42:
        centered_rectangle(com[1], com[2], com[3], com[4])
    elif op_code == 44:
        hollow_rectangle(com[1], com[2], com[3], com[4])
    elif op_code == 60:
        filled_ellipse(com[1], com[2], com[3], com[4])
    elif op_code == 61:
        ellipse(com[1], com[2], com[3], com[4])
    elif op_code == 62:
        filled_circle(com[1], com[2], com[3])
    elif op_code == 63:
        circle(com[1], com[2], com[3])

func draw_graphics_command_list(com_list: Array):
    for com in com_list:
        draw_graphics_command(com)

var to_draw = []
#     [35, 0.0, 0.0, 0.0, 20],
#     [35, 0.0, 20.0, 20, 20]
# ]

func demo1():
    pen_color = Color.BLACK
    pen_width = 1.0

    #draw_set_transform_matrix(Transform2D.FLIP_Y)
    line_segment(-1000, 0, 1000, 0)
    line_segment(0, -1000, 0, 1000)


    #draw_line(Vector2(-1000, 0), Vector2(1000, 0), Color.BLACK, 1.0)
    #draw_line(Vector2(0, -1000), Vector2(0, 1000), Color.BLACK, 1.0)

    change_graphics_color(Color.BLUE)
    centered_rectangle(100, 100, 50, 50)
    centered_rectangle(0, 0, 100, 100)
    hollow_rectangle(175, 100, 50, 50)
    change_graphics_color(Color.ORANGE_RED)
    change_pen_width(5)
    circle(250, 100, 25)
    filled_circle(325, 100, 25)

    centered_string(0, 175, "Hello Boxer Godot Center")
    left_string(0, 200, "Hello Boxer Godot Left")
    right_string(0, 225, "Hello Boxer Godot Right")


    draw_rect(Rect2(1.0, 1.0, 3.0, 3.0), Color.BLUE)
    draw_rect(Rect2(5.5, 1.5, 2.0, 2.0), Color.BLUE, false, 1.0)
    draw_rect(Rect2(9.0, 1.0, 5.0, 5.0), Color.BLUE)
    draw_rect(Rect2(16.0, 2.0, 3.0, 3.0), Color.BLUE, false, 2.0)

func demo_perf_lag():
    var p1 = Vector2(0, 0)
    var p2 = Vector2(1, 1)

    pen_color = Color.BLACK
    pen_width = 1.0

    for i in range(100):
        for j in range(100):
            line_segment(i, j, i+1, j)
        #line_segment(p1.x, p1.y, p2.x, p2.y)
        #p1 = p2
        #p2 = p1 + Vector2(2, 1)

var total = 0
var drawn = false
var captured = false

func _process(_delta):
    await RenderingServer.frame_post_draw
    if !captured and drawn:
        captured = true
        emit_signal("done_drawing")
        #queue_redraw()



func _draw():
    draw_graphics_command_list(to_draw)

    # demo1()
    # if !captured:
    #     demo1()
    #     drawn = true

    #demo_perf_lag()

func _ready() -> void:
    pass
