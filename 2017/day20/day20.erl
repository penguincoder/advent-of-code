-module(day20).
-export([start/0, simulate_particle/6]).

process_input_string(Input)
  -> Lines = re:split(Input, "\n"),
     process_input_lines(Lines, 0, []).

process_input_lines([], _Index, Particles)
  -> Particles;
process_input_lines([Line|Tail], Index, Particles)
  -> [Position,Velocity,Acceleration|_] = re:split(Line, ", "),
     PositionTuple = process_input_coordinate(Position),
     VelocityTuple = process_input_coordinate(Velocity),
     AccTuple = process_input_coordinate(Acceleration),
     process_input_lines(Tail, Index + 1, lists:append(Particles, [{ Index, PositionTuple, VelocityTuple, AccTuple }])).

process_input_coordinate(RawCoord)
  -> Coord = string:slice(RawCoord, 3, string:length(RawCoord) - 4),
     [X,Y,Z|_] = re:split(Coord, ","),
     { XInt, _ } = string:to_integer(X),
     { YInt, _ } = string:to_integer(Y),
     { ZInt, _ } = string:to_integer(Z),
     { XInt, YInt, ZInt }.

find_closest_particle(Particles)
  -> spawn_processors(Particles),
     get_processor_output(length(Particles)).

get_processor_output(Iterations)
  -> get_processor_output(Iterations, { -1, -1 }).

get_processor_output(Iterations, Result)
  when Iterations == 0 -> Result;
get_processor_output(Iterations, Result)
  -> { _MinIndex, MinDistance } = Result,
     receive
       { Index, Distance } when MinDistance == -1; Distance < MinDistance -> NewResult = { Index, Distance }, get_processor_output(Iterations - 1, NewResult);
       { _Index, _Distance } -> get_processor_output(Iterations - 1, Result)
     end.

spawn_processors(Particles)
  -> Iterations = 1000,
     spawn_processors(Particles, Iterations).

spawn_processors([], _Iterations)
  -> true;
spawn_processors([Head|Tail], Iterations)
  -> { Index, Position, Velocity, Acceleration } = Head,
     spawn(?MODULE, simulate_particle, [self(), Index, Position, Velocity, Acceleration, Iterations]),
     spawn_processors(Tail, Iterations).

simulate_particle(Parent, Index, Position, _Velocity, _Acceleration, Iterations)
  when Iterations == 0 -> { X, Y, Z } = Position,
                          Distance = abs(X) + abs(Y) + abs(Z),
                          Parent ! { Index, Distance };
simulate_particle(Parent, Index, Position, Velocity, Acceleration, Iterations)
  -> { PX, PY, PZ } = Position,
     { VX, VY, VZ } = Velocity,
     { AX, AY, AZ } = Acceleration,
     NewVX = AX + VX,
     NewVY = AY + VY,
     NewVZ = AZ + VZ,
     NewV = { NewVX, NewVY, NewVZ },
     NewPX = NewVX + PX,
     NewPY = NewVY + PY,
     NewPZ = NewVZ + PZ,
     NewP = { NewPX, NewPY, NewPZ },
     simulate_particle(Parent, Index, NewP, NewV, Acceleration, Iterations - 1).

calc_collisions(Iterations, _Positions, BadParticles, TotalParticleCount)
  when Iterations == 0 -> io:fwrite("found ~p collided particles of ~p total particles, ~p remain~n", [length(BadParticles), TotalParticleCount, TotalParticleCount - length(BadParticles)]), BadParticles;
calc_collisions(Iterations, Positions, BadParticles, TotalParticleCount)
  -> %io:fwrite("calculating collisions on ~p~n", [Iterations]),
     { CheckPositions, NextPositions } = lists:partition(fun(X) -> { _Index, Iteration, _Position } = X, Iteration == Iterations end, Positions),
     %io:fwrite("check positions ~p~n", [CheckPositions]),
     { _, FilteredPositions } = lists:partition(fun(X) -> { Index, _Iteration, _Position } = X, lists:any(fun(Y) -> Y == Index end, BadParticles) end, CheckPositions),
     CollidedPositions = lists:filter(fun(CheckPosition) ->
                                          { Index, _Iteration, Position } = CheckPosition,
                                          { X, Y, Z } = Position,
                                          lists:any(fun(NextPosition) ->
                                                        { OtherIndex, _OtherIteration, OtherPosition } = NextPosition,
                                                        { OtherX, OtherY, OtherZ } = OtherPosition,
                                                        (Index /= OtherIndex) and (OtherX == X) and (OtherY == Y) and (OtherZ == Z)
                                                    end, CheckPositions)
                                      end, FilteredPositions),
     %io:fwrite("collidedpositions ~p~n", [CollidedPositions]),
     Found = lists:map(fun(X) -> { Index, _Iteration, _Position } = X, Index end, CollidedPositions),
     calc_collisions(Iterations - 1, NextPositions, lists:append(BadParticles, Found), TotalParticleCount).

get_all_positions(_Particle, Steps, Positions)
  when Steps == 0 -> Positions;
get_all_positions(Particle, Steps, Positions)
  -> { Index, Position, Velocity, Acceleration } = Particle,
     { PX, PY, PZ } = Position,
     { VX, VY, VZ } = Velocity,
     { AX, AY, AZ } = Acceleration,
     NewVX = AX + VX,
     NewVY = AY + VY,
     NewVZ = AZ + VZ,
     NewV = { NewVX, NewVY, NewVZ },
     NewPX = NewVX + PX,
     NewPY = NewVY + PY,
     NewPZ = NewVZ + PZ,
     NewP = { NewPX, NewPY, NewPZ },
     get_all_positions({ Index, NewP, NewV, Acceleration }, Steps - 1, lists:append(Positions, [{ Index, Steps, NewP }])).

find_collided_particles(Particles)
  -> Iterations = 1000,
     Positions = lists:flatten(lists:map(fun(X) -> get_all_positions(X, Iterations, []) end, Particles)),
     calc_collisions(Iterations, Positions, [], length(Particles)).

start()
  -> Sample = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>",
     io:fwrite("closest particle sample ~p~n", [find_closest_particle(process_input_string(Sample))]),
     { ok, File } = file:read_file("input"),
     ProblemInput = process_input_string(unicode:characters_to_list(string:chomp(File))),
     io:fwrite("closest particle problem input ~p~n", [find_closest_particle(ProblemInput)]),
     Sample2 = "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>",
     io:fwrite("collided particle sample ~p~n", [find_collided_particles(process_input_string(Sample2))]),
     io:fwrite("collided particle problem input ~p~n", [find_collided_particles(ProblemInput)]).
