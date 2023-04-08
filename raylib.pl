:- module(raylib, [
    init_window/3,
    window_should_close/0,
    close_window/0,
    set_target_fps/1,
    get_screen_width/1,
    is_key_down/1,
    begin_drawing/0,
    end_drawing/0,
    clear_background/1,
    draw_text/5,
    draw_circle/4,
    draw_circle_v/3,
    draw_circle_gradient/5,
    draw_circle_lines/4,
    draw_rectangle/5,
    draw_rectangle_gradient_h/6,
    draw_rectangle_lines/5,
    draw_triangle/4,
    draw_triangle_lines/4,
    draw_poly/5,
    draw_poly_lines/5,
    draw_poly_lines_ex/6,
    load_texture/2,
    draw_texture/4,
    unload_texture/1,
    get_time/1
]).

:- use_module(library(ffi)).

:- initialization(raylib_load).

raylib_load :-
    foreign_struct(color, [uint8, uint8, uint8, uint8]),
    foreign_struct(vector2, [f32, f32]),
    foreign_struct(texture, [uint32, sint32, sint32, sint32, sint32]),
    use_foreign_module("./libraylib.so", [
	'InitWindow'([sint32, sint32, cstr], void),
	'WindowShouldClose'([], bool),
	'SetTargetFPS'([sint32], void),
	'GetScreenWidth'([], sint32),
	'BeginDrawing'([], void),
	'EndDrawing'([], void),
	'IsKeyDown'([sint32], bool),
	'ClearBackground'([color], void),
	'DrawText'([cstr, sint32, sint32, sint32, color], void),
	'DrawCircle'([sint32, sint32, f32, color], void),
	'DrawCircleV'([vector2, f32, color], void),
	'DrawCircleGradient'([sint32, sint32, f32, color, color], void),
	'DrawCircleLines'([sint32, sint32, f32, color], void),
	'DrawRectangle'([sint32, sint32, sint32, sint32, color], void),
	'DrawRectangleGradientH'([sint32, sint32, sint32, sint32, color, color], void),
	'DrawRectangleLines'([sint32, sint32, sint32, sint32, color], void),
	'DrawTriangle'([vector2, vector2, vector2, color], void),
	'DrawTriangleLines'([vector2, vector2, vector2, color], void),
	'DrawPoly'([vector2, sint32, f32, f32, color], void),
	'DrawPolyLines'([vector2, sint32, f32, f32, color], void),
	'DrawPolyLinesEx'([vector2, sint32, f32, f32, f32, color], void),
        'CloseWindow'([], void),
	'LoadTexture'([cstr], texture),
	'DrawTexture'([texture, sint32, sint32, color], void),
	'UnloadTexture'([texture], void),
	'GetTime'([], f64)
    ]).

color(color(A, B, C, D), ["color", A, B, C, D]).
color(lightgray, ["color", 200, 200, 200, 255]).
color(gray, ["color", 130, 130, 130, 255]).
color(darkgray, ["color", 80, 80, 80, 255]).
color(yellow, ["color", 253, 249, 0, 255]).
color(gold, ["color", 255, 203, 0, 255]).
color(orange, ["color", 255, 161, 0, 255]).
color(pink, ["color", 255, 109, 194, 255]).
color(red, ["color", 230, 41, 55, 255]).
color(maroon, ["color", 190, 33, 55, 255]).
color(green, ["color", 0, 228, 48, 255]).
color(lime, ["color", 0, 158, 47, 255]).
color(darkgreen, ["color", 0, 117, 44, 255]).
color(skyblue, ["color", 102, 191, 255, 255]).
color(blue, ["color", 0, 121, 241, 255]).
color(darkblue, ["color", 0, 82, 172, 255]).
color(purple, ["color", 200, 122, 255, 255]).
color(violet, ["color", 135, 60, 190, 255]).
color(darkpurple, ["color", 112, 31, 126, 255]).
color(beige, ["color", 211, 176, 131, 255]).
color(brown, ["color", 127, 106, 79, 255]).
color(darkbrown, ["color", 76, 63, 47, 255]).
color(white, ["color", 255, 255, 255, 255]).
color(black, ["color", 0, 0, 0, 255]).
color(blank, ["color", 0, 0, 0, 0]).
color(magenta, ["color", 255, 0, 255, 255]).
color(raywhite, ["color", 245, 245, 245, 255]).

vector(vector(X, Y), ["vector2", X, Y]).

key(right, 262).
key(left, 263).
key(down, 264).
key(up, 265).

init_window(Width, Height, Title) :- ffi:'InitWindow'(Width, Height, Title).
window_should_close :- ffi:'WindowShouldClose'.
close_window :- ffi:'CloseWindow'.
set_target_fps(A) :- ffi:'SetTargetFPS'(A).
begin_drawing :- ffi:'BeginDrawing'.
end_drawing :- ffi:'EndDrawing'.
is_key_down(Key0) :- key(Key0, Key), ffi:'IsKeyDown'(Key).
get_screen_width(A) :- ffi:'GetScreenWidth'(A).
    
clear_background(Color0) :- color(Color0, Color), ffi:'ClearBackground'(Color).
draw_text(Text, A, B, C, Color0) :- color(Color0, Color), ffi:'DrawText'(Text, A, B, C, Color).
draw_circle(A, B, C, Color0) :- color(Color0, Color), ffi:'DrawCircle'(A, B, C, Color).
draw_circle_v(A0, B, Color0) :-
    vector(A0, A),
    color(Color0, Color),
    ffi:'DrawCircleV'(A, B, Color).
draw_circle_gradient(A,B,C,Color0, Color1) :- color(Color0, Color2), color(Color1, Color3), ffi:'DrawCircleGradient'(A,B,C,Color2, Color3).
draw_circle_lines(A, B, C, Color0) :- color(Color0, Color), ffi:'DrawCircleLines'(A, B, C, Color).
draw_rectangle(A,B,C,D,Color0) :- color(Color0, Color), ffi:'DrawRectangle'(A,B,C,D,Color).
draw_rectangle_gradient_h(A,B,C,D,Color0,Color1) :- color(Color0, Color2), color(Color1, Color3), ffi:'DrawRectangleGradientH'(A,B,C,D,Color2,Color3).
draw_rectangle_lines(A,B,C,D,Color0) :- color(Color0, Color), ffi:'DrawRectangleLines'(A,B,C,D,Color).
draw_triangle(A0,B0,C0,Color0) :-
    vector(A0, A),
    vector(B0, B),
    vector(C0, C),
    color(Color0, Color),
    ffi:'DrawTriangle'(A, B, C, Color).
draw_triangle_lines(A0,B0,C0,Color0) :-
    vector(A0, A),
    vector(B0, B),
    vector(C0, C),
    color(Color0, Color),
    ffi:'DrawTriangleLines'(A, B, C, Color).
draw_poly(A0, B, C, D, Color0) :-
    vector(A0, A),
    color(Color0, Color),
    ffi:'DrawPoly'(A, B, C, D, Color).
draw_poly_lines(A0, B, C, D, Color0) :-
    vector(A0, A),
    color(Color0, Color),
    ffi:'DrawPolyLines'(A, B, C, D, Color).
draw_poly_lines_ex(A0, B, C, D, E, Color0) :-
    vector(A0, A),
    color(Color0, Color),
    ffi:'DrawPolyLinesEx'(A, B, C, D, E, Color).

load_texture(A, B) :- ffi:'LoadTexture'(A, B).
draw_texture(A, B, C, D0) :- color(D0, D), ffi:'DrawTexture'(A, B, C, D).
unload_texture(A) :- ffi:'UnloadTexture'(A).
get_time(Time) :- ffi:'GetTime'(Time).
