![Grade A+](https://img.shields.io/badge/Grade-A%2B-green?color=009F00)
# Final Examination: Algorithms and Data Structures Project

This is one of the three project for BSc thesis at **Politecnico di Milano**.  
The related course for this project is _"Algoritmi e Principi dell'Informatica"_ (i.e. Algorithms and Theoretical Computer Science).

**NOTE:** Project's main concern was about performance.

## Specifications

Aim of this project was to realize a basic social network analyzer in which every _entity_ can be linked to another by one 
or more _connection type_. Every time it is asked by a `report` command, a report must show for each _connection type_ 
the _"hole"_ in which the highest number of _connections_ go into.

### Example
If
* "Alice" is sister to "Bob";
* "Clara" is sister to "Bob";
* "Diana" is sister to "Eleonore";

then, when printing information about the _connection type_ "sister", just "Bob" must be shown (because he has two 
_incoming logic relationships_, and "Eleonore" has got just one of them).

See specification here: [Specification document](ProvaFinale2019.pdf) (italian only).  
See the code in `main.c` file.

## Implementation

My implementation is mainly focused on raw **speed of execution**. Of course lots of attention has been paid to lowering memory 
consumption as well, but my primarily concern when I designed the program was to achieve as much speed of execution as 
possible.  
Some of my priorities when I engineered my program:
- Proper **data structure** usage in order to reduce time complexity when performing repetitive operations
- Fast **hash algorithm**
- Proper **memory usage** (**cache exploitation** as much as possible)
- Attention to what specific C functions are called (especially for I/O operations, which were the bottlenecks at runtime)
- Taking shortcuts in computations when possible (e.g. when parsing the command string taken in input).

### More implementation info

**\[Taken from the comment on top of the code\]**

#### High-level design
- Particular data structures needed to be used in order to run the program as fast as possible.
- My choice was to use a mix of **Hash Tables** and **RB-Trees** thus to have optimum time complexity.
- Memory usage was critical. In order not to exceed memory bounds, foreach entity id and relation_type id, I save just 
one char vector, and I refer to them using pointers.

#### Low-level design
- **Low-level optimizations** all over the code is also a key factor, which allowed me to speed up the entire process in a significant way.
- I tried to **reduce I/O and memory interaction** as much as possible (especially for I/O, which occurred to be a huge 
bottleneck when it came to use some particular functions like `printf()`).
- Memory interaction was also reduced in two ways: by **not doing** some operations which involved **strings comparison**, 
and by adopting proper memory management practices (both for **alignment** and for **caching**).
As an example: entities' **hash table was sized** in order to keep the entire vector **in one memory page** (other than to have a good load factor).
Considering a `64bit` processor, size of pointers is about `8 Byte`, so the table dimension is `8*499 = 3992 Byte`,
so the table can be stored in a single `4KB` page (Linux OS default dimension).
This kind of optimizations allow the process to **better exploit cache memory** as much as possible, and so to run faster.
- Another major improvement is made possible by the way RB-trees are used: it is not the ASCII order to be used for element insertion,
by this way I would have been forced to use `strcmp()` which has a **O(N)** complexity (`N`: string dimension) and so to waste time;
instead of that, I went for a poor-semantic but way better **O(1)** solution. Key problem is to just store nodes in **O(logN)** time,
this because in my RB-trees **ASCII order is unnecessary**, so I just take the entity's node memory address (from the hash table) and use it
as a parameter to insert an entity-related node in the RB-tree. This allows me **not to do slow string comparisons** and to use
`8 Byte` **unsigned integer comparisons**, which **processors can probably handle in few clock cycles** (matter of nanoseconds).
By using RB-trees this way I can also make them behave like a **kind of randomized tree**, and so to **keep the structure safe from
time complexity systematic attack**. With ASCII order insertion, an attacker could choose particular strings in order to make dictionary
operations work at their worst (`logN` comparisons); this is **not possible if we use addresses as parameters to insert nodes**: an attacker cannot know
which strings are stored in RB's leafs since he/she cannot know the entity node's address without analyzing the memory first.
(What is said above, a part from the report-tree, in which string comparisons were necessary).
- Some of the optimizations I did in order to speed up the program may have made it to lack semantic in some parts.
However it is not a problem: **evaluation tests were made automatically by a computer**, so I focused on raw speed and memory usage.
If the evaluation was made by humans, I would probably decided not to write certain kind of code, in order to maintain both semantic and flexibility.
- **NOTE:** Just `glibc` functions and few others (like `strdup()`) were allowed to be used.

---

### Final grade: 30 cum Laude / 30
###### Academic Year: 2018/2019
