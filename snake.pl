:- use_module(raylib).

:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(time)).
:- use_module(library(format)).
:- use_module(library(random)).
:- use_module(library(clpz)).

% State: Snake, Food, Time, Direction, LookAt, Ticks, End
start :-
    init_window(400, 400, "Snake"),
    set_target_fps(60),
    get_time(Time),
    loop(state([10-10, 10-11], [], Time, up, up, 0, false)).

loop(State0) :-
    (
	window_should_close ->
	close_window,
	halt
    ;   (
	inputs(Inputs),!,
	update(Inputs, State0, State),!,
	render(State),!,
	loop(State)
        )
    ).

inputs(Inputs) :-
    phrase(inputs_, Inputs).

inputs_ -->
    inputs_right_,
    inputs_left_,
    inputs_up_,
    inputs_down_.

inputs_right_ -->
    (
	{ is_key_down(right) },
	[right]
    ) | [].

inputs_left_ -->
    (
	{ is_key_down(left) },
	[left]
    ) | [].

inputs_up_ -->
    (
	{ is_key_down(up) },
	[up]
    ) | [].

inputs_down_ -->
    (
	{ is_key_down(down) },
	[down]
    ) | [].

update(Inputs, State0, State) :-
    \+ update_needed(State0, _),
    State0 = state(Snake, Food, T, Dir0, LookAt, Ticks, false),
    State = state(Snake, Food, T, Dir, LookAt, Ticks, false),
    snake_dir(Inputs, Dir0, LookAt, Dir).

update(Inputs, State0, State) :-
    update_needed(State0, T),
    State0 = state(Snake0, Food0, _, Dir0, LookAt0, Ticks0, false),
    State = state(Snake, Food, T, Dir, Dir, Ticks, End),
    Ticks is Ticks0 + 1,
    snake_dir(Inputs, Dir0, LookAt0, Dir),
    move_snake(Dir, Snake0, Snake1),
    (
	crash_snake(Snake1) ->
	(
	    End = true,
	    Snake1 = Snake
	)
    ;   (
	End = false,
	food_update(Ticks, Snake1, Food0, Food1),
	eat_update(Snake1, Snake, Food1, Food)
        )
    ).

update(_Inputs, State, State) :-
    State = state(_, _, _, _, _, _, true).

update_needed(State0, Time1) :-
    State0 = state(_, _, Time0, _, _, _, _),
    get_time(Time1),
    Time1 > Time0 + 0.3 .

snake_dir(Inputs, _, LookAt, left) :-
    member(left, Inputs),
    \+ member(right, Inputs),
    LookAt \= right,!.

snake_dir(Inputs, _, LookAt, right) :-
    member(right, Inputs),
    \+ member(left, Inputs),
    LookAt \= left,!.

snake_dir(Inputs, _, LookAt, up) :-
    member(up, Inputs),
    \+ member(down, Inputs),
    LookAt \= down,!.

snake_dir(Inputs, _, LookAt, down) :-
    member(down, Inputs),
    \+ member(up, Inputs),
    LookAt \= up,!.

snake_dir(_Inputs, Current, LookAt, Current).

crash_snake([SnakeHead|SnakeTail]) :-
    member(SnakeHead, SnakeTail).

move_snake(left, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewX is (X - 1) mod 20,
    Snake = [NewX-Y,SnakeHead|SnakeTail].

move_snake(right, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewX is (X + 1) mod 20,
    Snake = [NewX-Y,SnakeHead|SnakeTail].

move_snake(up, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewY is (Y - 1) mod 20,
    Snake = [X-NewY,SnakeHead|SnakeTail].

move_snake(down, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewY is (Y + 1) mod 20,
    Snake = [X-NewY,SnakeHead|SnakeTail].

food_update(Ticks, Snake, Food0, Food) :-
    F is Ticks mod 15,
    (
	F = 1 ->
	(
	    food_position(Snake, X-Y),
	    Food = [X-Y|Food0]
	)
    ;   Food0 = Food
    ).

food_position(Snake, X-Y) :-
    setof(SnakePos, SnakeX^SnakeY^(
	      [SnakeX, SnakeY] ins 0..20, 
	      SnakePos = SnakeX-SnakeY,
	      label([SnakeX, SnakeY]),
	      \+ member(SnakePos, Snake)
	  ), FreeSpots),
    random_choice(FreeSpots, X-Y).

random_choice(Xs, X) :-
    length(Xs, N),
    random_integer(0, N, R),
    nth0(R, Xs, X).

eat_update(Snake0, Snake, Food0, Food) :-
    Snake0 = [SnakeHead|SnakeTail],
    Snake = [SnakeHead, SnakeHead|SnakeTail],
    select(SnakeHead, Food0, Food).

eat_update(Snake0, Snake, Food0, Food) :-
    Snake0 = [SnakeHead|SnakeTail],
    Snake0 = Snake,
    Food0 = Food,
    \+ member(SnakeHead, Food0).

render(State) :-
    begin_drawing,
    clear_background(color(255, 255, 255, 255)),
    State = state(Snake, Food, _, _, _, _, End),
    draw_snake(Snake),
    draw_food(Food),
    draw_score(Snake, End),
    end_drawing.

draw_snake(Snake) :-
    maplist(draw_snake_block, Snake).

draw_snake_block(X-Y) :-
    PixelX is X*20,
    PixelY is Y*20,
    draw_rectangle(PixelX, PixelY, 20, 20, red),
    draw_rectangle_lines(PixelX, PixelY, 20, 20, black).

draw_food(Food) :-
    maplist(draw_food_block, Food).

draw_food_block(X-Y) :-
    PixelX is X*20+10,
    PixelY is Y*20+10,
    draw_circle(PixelX, PixelY, 10, green),
    draw_circle_lines(PixelX, PixelY, 10, black).

draw_score(Snake, End) :-
    length(Snake, Score0),
    Score is Score0 - 2,
    phrase(format_("Score: ~d", [Score]), ScoreText),
    draw_text(ScoreText, 10, 10, 16, gray),
    (
	End = true ->
	draw_text("Game over!", 150, 180, 16, black)
    ; true
    ).

:- initialization(start).
