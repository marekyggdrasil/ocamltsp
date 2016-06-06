Example OCaml code,
Program solving Traveling Salesman Problem,

source can be found in `src/tspml.ml`
simply hit `make` to build the project

### Usage

`./tspml.native micro.inst`

solves instance from file `micro.inst`.

If you would like to create your own problem instance,
you can base it on `example.inst` file.

output : `4 3 1 2`
Means that from first city we move to fourth, from second to third,
from third to first and from fourth to second.

Software was tested on instances micro and medium, results were compared
with Concorde TSP solver and matched.

### Explanation

Please have a look at file `reasoning-ocamltsp.pdf` to see the reasoning behind this program.
