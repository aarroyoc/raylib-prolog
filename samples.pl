:- use_module(raylib).

hello_world :-
    init_window(800, 450, "Scryer Prolog"),
    hello_world_loop.

hello_world_loop :-
    (
	window_should_close ->
	close_window
    ;   (
	begin_drawing,
	clear_background(color(245, 245, 245, 255)),
	draw_text("Congrats! You created your first window!", 190, 200, 20, color(200, 200, 200, 255)),
	end_drawing,
	hello_world_loop
	)
    ).

shapes :-
    init_window(800, 450, "Scryer Prolog - Raylib shapes"),
    shapes_loop(0.0).

shapes_loop(Rotation0) :-
    (
	window_should_close ->
	close_window
    ;   (
	Rotation is Rotation0 + 0.2,
	begin_drawing,
	clear_background(color(245, 245, 245, 255)),
	draw_text("some basic shapes available on raylib", 20, 20, 20, darkgray),
	draw_circle(160, 120, 35, darkblue),
	draw_circle_gradient(160, 220, 60, green, skyblue),
	draw_circle_lines(160, 340, 80, darkblue),
	draw_rectangle(340, 100, 120, 60, red),
	draw_rectangle_gradient_h(310, 170, 180, 130, maroon, gold),
	draw_rectangle_lines(360, 320, 80, 60, orange),
	draw_triangle(vector(600, 80.0), vector(540, 150), vector(660, 150), violet),
	draw_triangle_lines(vector(600, 160), vector(580, 230), vector(620, 230), darkblue),
	draw_poly(vector(600, 330), 6, 80, Rotation, brown),
	draw_poly_lines(vector(600, 330), 6, 90, Rotation, brown),
	draw_poly_lines_ex(vector(600, 330), 6, 85, Rotation, 6, beige),
	end_drawing,
	shapes_loop(Rotation)
	)
    ).

core_input_keys :-
    init_window(800, 450, "Scryer Prolog - Core Input Keys"),
    set_target_fps(60),
    core_input_keys_loop(vector(400, 225)).

core_input_keys_loop(vector(X0, Y0)) :-
    (
	window_should_close ->
	close_window
    ;   (is_key_down(right) -> X1 is X0 + 2; X1 = X0),
	(is_key_down(left) -> X2 is X1 - 2; X2 = X1),
	(is_key_down(up) -> Y1 is Y0 - 2; Y1 = Y0),
	(is_key_down(down) -> Y2 is Y1 + 2; Y2 = Y1),
	BallPosition = vector(X2, Y2),
        begin_drawing,
	clear_background(raywhite),
	draw_text("move the ball with arrow keys", 10, 10, 20, darkgray),
	draw_circle_v(BallPosition, 50, maroon),
	end_drawing,
	core_input_keys_loop(BallPosition)
    ).

texture :-
    init_window(800, 450, "Scryer Prolog - Texture"),
    set_target_fps(60),
    load_texture("raylib.png", Texture),
    texture_loop(Texture).

texture_loop(Texture) :-
    (
	window_should_close ->
	close_window
    ;   begin_drawing,
	clear_background(raywhite),
	draw_texture(Texture, 20, 20, white),
	draw_text("this IS a texture!", 360, 370, 10, gray),
	end_drawing,
	texture_loop(Texture)
    ).
