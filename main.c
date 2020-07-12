/*
 * ANDREA ALTOMARE 891365 - 2019
 * Progetto di Algoritmi e Strutture Dati
 * Ingegneria Informatica, Politecnico di Milano
 *
 * ##### ABSTRACT #####
 * - The project's aim is to evaluate the speed and memory usage of student's program, other than the correctness.
 *
 * ## High-level design ##
 * - Particular data structures needed to be used in order to run the program as fast as possible.
 * - My choice was to use a mix of Hash Tables and RB-Trees thus to have optimum time complexity.
 * - Memory usage was critical. In order not to exceed memory bounds, foreach entity id and relation_type id, I save just one char vector,
 * and I referred to them using pointers.
 *
 * ## Low-level design ##
 * - Low-level optimizations all over the code is also a key factor, which allowed me to speed up the entire process in a significant way.
 * - I tried to reduce I/O and memory interaction as much as possible
 * (expecially for I/O, which occurred to be a huge bottleneck when it came to use some particular functions like printf()).
 * - Memory interaction was also reduced in two ways: by not doing some operations which involved strings comparison,
 * and by adopting proper memory management practices (both for alignment and for caching).
 * As an example: entities' hash table was sized in order to keep the entire vector in one memory page (other than to have a good load factor).
 * Considering a 64bit processor, size of pointers is about 8 Byte, so the table dimension is 8*499 = 3992 Byte,
 * so the table can be stored in a single 4KB page (Linux OS default dimension).
 * This kind of optimizations allow the process to better exploit cache memory as much as possible, and so to run faster.
 * - Another major improvement is made possible by the way RB-trees are used: it is not the ASCII order to be used for element insertion,
 * by this way I would have been forced to use strcmp() which has a O(N) complexity (N: string dimension) and so to waste time;
 * instead of that, I went for a poor-semantic but way better O(1) solution. Key problem is to just store nodes in O(logN) time,
 * this because in my RB-trees ASCII order is unnecessary, so I just take the entity's node memory address (from the hash table) and use it
 * as a parameter to insert an entity-related node in the RB-tree. This allows me not to do slow string comparisons and to use
 * 8 Byte unsigned integer comparisons, which processors can probably handle in fews clock cycles (matter of nanoseconds).
 * By using RB-trees this way I can also make them behave like a kind of randomized tree, and so to keep the structure safe from
 * time complexity systematic attack. With ASCII order insertion, an attacker could choose particular strings in order to make dictionary
 * operations work at their worst (logN comparisons); this is not possible if we use addresses as parameters to insert nodes: an attacker cannot know
 * which strings are stored in RB's leafs since he/she cannot know the entity node's address without analyzing the memory first.
 * (What is said above, a part from the report-tree, in which string comparisons were necessary).
 *
 * - Some of the optimizations I did in order to speed up the program may have made it to lack semantic in some parts.
 * However it is not a problem: evaluation tests were made automatically by a computer, so I focused on raw speed and memory usage.
 * If the evaluation was made by humans, I would probably decided not to write certain kind of code, in order to maintain both semantic and flexibility.
 * - NB: Just glibc functions and few others (like strdup()) were allowed to be used.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define TRUE 1
#define FALSE 0
//#define YES 'Y' // true
//#define NO 'N' // false
#define INPUT_BUFFER_SIZE 200
#define ID_BUFFER_SIZE 50
#define BLACK 'B' // color for RB-Tree
#define RED 'R' // color for RB-Tree
#define FNV_OFFSET_BASIS 2166136261
#define FNV_PRIME 16777619
#define REL_TYPE_TABLE_SIZE 19
#define ENTITY_TABLE_SIZE 499
//#define ENTITY_TABLE_SIZE 6143
//#define ENTITY_TABLE_SIZE 19997

/* DATA STRUCTURES DEFINITION */
// list node for relation destinations. !NB: This structure was used to speed up the entity deletion process. Cancelled because of memory bounds exceeding.
/*typedef struct _destination {
    struct _destination *next;
    struct _destination *previous;
    void *source_ent;
}destination_t;*/

// list node struct for relation hash table
typedef struct _relation_type {
    struct _relation_type *next;
    struct _relation_type *previous;
    char *id; // id_rel
    int maximum;
    //char max_modified; // bool variable: tells if the maximum has been modified
}rel_type_t;

// RB-Tree node for relation receivings (holes)
typedef struct _receiving {
    struct _receiving *p;
    struct _receiving *left;
    struct _receiving *right;
    rel_type_t *rel; // pointer to the relation node
    int rec_count; // number of the receiving relations for a given type of relation
    char color;
}receiving_t;

// RB-Tree node for relations source
typedef struct _source {
    struct _source *p;
    struct _source *left;
    struct _source *right;
    void *ent_dest; // pointer to entity of the relation receiving entity
    rel_type_t *rel; // relation id which is being send
    receiving_t *ent_hole; // pointer to the entity's relation type hole
    char color;
}source_t;

// list node struct for entity hash table
typedef struct _entity {
    struct _entity *next;
    struct _entity *previous;
    receiving_t *rec_root; // pointer to Receiving RB's root
    source_t *source_root; // pointer to source RB's root
    char *id; // id_ent
}entity_t;

// node struct for report RB-tree
typedef struct _report_node {
    struct _report_node *p;
    struct _report_node *left;
    struct _report_node *right;
    rel_type_t *rel;
    entity_t *ent;
    int rec_count;
    char color;
}report_node_t;

// auxiliary structure for report function
typedef struct _last {
    char id_rel[ID_BUFFER_SIZE];
    int num;
}last_t;

/* FUNCTION PROTOTYPES */
// HASH TABLES FUNCTIONS
unsigned int hf(char *key); // hash function: returns an array index
entity_t* entity_hash_table_search(entity_t *T[], char *key); // search into the hash table, then return a pointer (undefined type for flexibility)
void entity_hash_table_insert(entity_t *T[], entity_t **node); // insert a node into the hash table
void entity_hash_table_delete(entity_t *T[], entity_t *node); // delete a node from the hash table

rel_type_t* rel_type_hash_table_search(rel_type_t *T[], char *key); // search into the hash table, then return a pointer (undefined type for flexibility)
void rel_type_hash_table_insert(rel_type_t *T[], rel_type_t **node); // insert a node into the hash table
void rel_type_hash_table_delete(rel_type_t *T[], rel_type_t *node); // delete a node from the hash table

// LIST FUNCTIONS (needed because of chained-hashing)
entity_t* entity_list_search(entity_t *L, char *key); // search into the chained list, then return a pointer (undefined type for flexibility)
void entity_list_insert(entity_t **L, entity_t **node); // insert a node into the list
void entity_list_delete(entity_t **L, entity_t *node); // delete a node from the list

rel_type_t* rel_type_list_search(rel_type_t *L, char *key); // search into the chained list, then return a pointer (undefined type for flexibility)
void rel_type_list_insert(rel_type_t **L, rel_type_t **node); // insert a node into the list
void rel_type_list_delete(rel_type_t **L, rel_type_t *node); // delete a node from the list

/*destination_t* destination_list_search(destination_t *L, void *ent); // search into the chained list, then return a pointer (undefined type for flexibility)
void destination_list_insert(destination_t **L, destination_t **node); // insert a node into the list
void destination_list_delete(destination_t **L, destination_t *node); // delete a node from the list*/

// RB-TREE FUNCTIONS
void source_left_rotate(source_t **T, source_t *node); // left rotation for adjusting tree
void source_right_rotate(source_t **T, source_t *node); // right rotation for adjusting tree
// RB search functions
source_t* source_RB_search(source_t *x, rel_type_t *key, void* ent);
source_t* source_RB_search_entity(source_t *x, void* ent);
void source_RB_delete_entity(source_t **T, void* ent); // deletes every relation which has a give entity as its receiving one
//void source_inorder_tree_walk(source_t *x);
// RB insertion functions
void source_RB_insert(source_t **T, source_t *node);
void source_RB_insert_fixup(source_t **T, source_t *node);
// RB deletion functions
source_t* source_tree_minimum(source_t *node);
void source_RB_transplant(source_t **T, source_t *node1, source_t *node2);
void source_RB_delete(source_t **T, source_t *node);
void source_RB_delete_fixup(source_t **T, source_t *node);

void receiving_left_rotate(receiving_t **T, receiving_t *node); // left rotation for adjusting tree
void receiving_right_rotate(receiving_t **T, receiving_t *node); // right rotation for adjusting tree
// RB search function
receiving_t* receiving_RB_search(receiving_t *x, rel_type_t *key);
//void receiving_inorder_tree_walk(receiving_t *x);
// RB insertion functions
void receiving_RB_insert(receiving_t **T, receiving_t *node);
void receiving_RB_insert_fixup(receiving_t **T, receiving_t *node);
// RB deletion functions
receiving_t* receiving_tree_minimum(receiving_t *node);
void receiving_RB_transplant(receiving_t **T, receiving_t *node1, receiving_t *node2);
void receiving_RB_delete(receiving_t **T, receiving_t *node);
void receiving_RB_delete_fixup(receiving_t **T, receiving_t *node);

void report_node_left_rotate(report_node_t **T, report_node_t *node); // left rotation for adjusting tree
void report_node_right_rotate(report_node_t **T, report_node_t *node); // right rotation for adjusting tree
// RB search functions
report_node_t* report_node_RB_search(report_node_t *x, rel_type_t *key, entity_t* ent);
report_node_t* report_node_RB_search_relation(report_node_t *x, rel_type_t *rel); // search for any node which is associated with a given relation type
void report_node_RB_delete_relation(report_node_t **T, rel_type_t* rel); // deletes every relation which has a give entity as its receiving one
void report_node_inorder_tree_walk(report_node_t *x);
// RB insertion functions
void report_node_RB_insert(report_node_t **T, report_node_t *node);
void report_node_RB_insert_fixup(report_node_t **T, report_node_t *node);
// RB deletion functions
report_node_t* report_node_tree_minimum(report_node_t *node);
void report_node_RB_transplant(report_node_t **T, report_node_t *node1, report_node_t *node2);
void report_node_RB_delete(report_node_t **T, report_node_t *node);
void report_node_RB_delete_fixup(report_node_t **T, report_node_t *node);

// PROJECT FUNCTIONS
// Main functions
void addent(char *ent); // adds an entity
void addrel(char *ent_source, char *ent_receiving, char *id_rel); // adds a relation
void delent(char *id_ent); // deletes an entity
void delrel(char *ent_source, char *ent_receiving, char *id_rel); // deletes a relation
void report(); // report
// Auxiliary functions
rel_type_t* new_rel_type(char *id_rel); // create a new relation type using its hash table
void add_rel_instance(entity_t *src, entity_t *dst, rel_type_t *rel); // add anew relation instance between two entities
int search_maximum(rel_type_t *rel); // search for the new maximum for a given relation type
void add_maximum(rel_type_t *rel); // add new maximum node to the report tree for a given relation type
void delete_source_relations(source_t **T); // delete every node (and other operations) in the source tree for a specific entity
void delete_destination_relations(entity_t *dst); // delete every source node in other entity's source-RB-Tree in which the given entity is a destination entity
void delete_holes(receiving_t **T, entity_t *ent); // delete every receiving node form the given entity's hole RB-tree root (recursively)

/* GLOBAL VARIABLES */
rel_type_t *rel_type_hash_t[REL_TYPE_TABLE_SIZE] = { NULL }; // hash table declaration and initialization
entity_t *entity_hash_t[ENTITY_TABLE_SIZE] = { NULL }; // hash table declaration and initialization
report_node_t *report_tree; // RB-tree for the report
source_t sNIL; // NIL node for source_t RB-tree
receiving_t rNIL; // NIL node for receiving_t RB-tree
report_node_t repNIL; // NIL node for  report_node_t RB-tree
last_t last; // last printed node
int rel_type_just_created; // boolean variable. Tells if a relation type has just been instanced in its hash table
// this lookup table helps just for improving raw speed: better this than itoa(); or printf("%d",number); functions
static const char *lookup[]={"0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425","426","427","428","429","430","431","432","433","434","435","436","437","438","439","440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455","456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471","472","473","474","475","476","477","478","479","480","481","482","483","484","485","486","487","488","489","490","491","492","493","494","495","496","497","498","499","500","501","502","503","504","505","506","507","508","509","510","511","512"};

/* MAIN FUNCTION */
int main() {
    char input[INPUT_BUFFER_SIZE]; // input string
    char ent_source[ID_BUFFER_SIZE]; // source entity id string
    char ent_receiving[ID_BUFFER_SIZE]; // receiving entity id string
    char rel[ID_BUFFER_SIZE]; // relation id string
    int index; // index for input parsing
    int index2; // index for string parsing

    /* SYSTEM INITIALIZATION */
    // global variables initialization
    rel_type_just_created=FALSE;

    // last initialization
    last.id_rel[0]='\0';
    last.num=0;

    // sNIL initialization
    sNIL.p=&sNIL;
    sNIL.left=&sNIL;
    sNIL.right=&sNIL;
    sNIL.ent_dest=NULL;
    sNIL.rel=NULL;
    sNIL.ent_hole=NULL;
    sNIL.color=BLACK;

    // rNIL initialization
    rNIL.p=&rNIL;
    rNIL.left=&rNIL;
    rNIL.right=&rNIL;
    rNIL.rel=NULL;
    rNIL.rec_count=0;
    rNIL.color=BLACK;

    // repNIL initialization
    repNIL.p=&repNIL;
    repNIL.left=&repNIL;
    repNIL.right=&repNIL;
    repNIL.rel=NULL;
    repNIL.ent=NULL;
    repNIL.rec_count=0;
    repNIL.color=BLACK;

    // report_tree initialization
    report_tree=&repNIL;
    /* END INITIALIZATION */

    /* PROGRAM CORE LOOP */
    while(TRUE) {
        // Command input
        fgets(input,INPUT_BUFFER_SIZE,stdin);

        // Flow control structure
        //result=memcmp(command,"addrel",COMMAND_BUFFER_SIZE);
        if(input[0] == 'a') {
            if(input[3] == 'r') {
                // addrel code
                /* Input parsing */
                // source entity
                index = 8;
                index2 = 0;
                do {
                    ent_source[index2] = input[index];
                    index++;
                    index2++;
                } while (input[index] != '"');
                ent_source[index2] = '\0'; // NULL terminator

                // destination entity
                index += 3;
                index2 = 0;
                do {
                    ent_receiving[index2] = input[index];
                    index++;
                    index2++;
                } while (input[index] != '"');
                ent_receiving[index2] = '\0'; // NULL terminator

                // relation id
                index += 3;
                index2 = 0;
                do {
                    rel[index2] = input[index];
                    index++;
                    index2++;
                } while (input[index] != '"');
                rel[index2] = '\0'; // NULL terminator
                /* End parsing */

                addrel(ent_source, ent_receiving, rel);
            }
            else {
                // addent code
                /* Input parsing */
                // source entity
                index=8;
                index2=0;
                do {
                    ent_source[index2]=input[index];
                    index++;
                    index2++;
                }while(input[index]!='"');
                ent_source[index2]='\0'; // NULL terminator
                /* End parsing */

                addent(ent_source);
            }
        }
        else {
            //result=memcmp(command,"report",COMMAND_BUFFER_SIZE);
            if(input[0] == 'r') {
                // report code
                report();
            }
            else {
                if (input[0] == 'd') {
                    //result=memcmp(command,"delrel",COMMAND_BUFFER_SIZE);
                    if (input[3] == 'r') {
                        // delrel code
                        /* Input parsing */
                        // source entity
                        index = 8;
                        index2 = 0;
                        do {
                            ent_source[index2] = input[index];
                            index++;
                            index2++;
                        } while (input[index] != '"');
                        ent_source[index2] = '\0'; // NULL terminator

                        // destination entity
                        index += 3;
                        index2 = 0;
                        do {
                            ent_receiving[index2] = input[index];
                            index++;
                            index2++;
                        } while (input[index] != '"');
                        ent_receiving[index2] = '\0'; // NULL terminator

                        // relation id
                        index += 3;
                        index2 = 0;
                        do {
                            rel[index2] = input[index];
                            index++;
                            index2++;
                        } while (input[index] != '"');
                        rel[index2] = '\0'; // NULL terminator
                        /* End parsing */

                        delrel(ent_source, ent_receiving, rel);
                    }
                    else {
                        // delent code
                        /* Input parsing */
                        // source entity
                        index = 8;
                        index2 = 0;
                        do {
                            ent_source[index2] = input[index];
                            index++;
                            index2++;
                        } while (input[index] != '"');
                        ent_source[index2] = '\0'; // NULL terminator
                        /* End parsing */

                        delent(ent_source);
                    }
                }
                else {
                    return 0; // end of input
                }
            }
        }
    }

    return 0;
}

// PROJECT FUNCTIONS
// Main functions
// adds an entity
void addent(char *ent) {
    entity_t *aux;

    //printf("addent \"%s\"\n",ent);
    aux=entity_hash_table_search(entity_hash_t,ent);

    if(aux == NULL) {
        // create a new entity instance
        aux=(entity_t *)malloc(sizeof(entity_t));
        aux->next=NULL;
        aux->previous=NULL;
        aux->rec_root=&rNIL;
        aux->source_root=&sNIL;
        aux->id=strdup(ent);

        // insert new entity in the hash table
        entity_hash_table_insert(entity_hash_t,&aux);
    }
    // if aux != NULL then entity does already exist: do nothing
}

// adds a relation
void addrel(char *ent_source, char *ent_receiving, char *id_rel) {
    entity_t *src; // source entity
    entity_t *dst; // destination entity
    rel_type_t *rel; // relation type

    //printf("addrel \"%s\" \"%s\" \"%s\"\n",ent_source,ent_receiving,rel);
    src=entity_hash_table_search(entity_hash_t,ent_source); // search for the source entity
    // continue only if it exists
    if(src!=NULL) {
        dst=entity_hash_table_search(entity_hash_t,ent_receiving); // search for the destination entity
        // continue only if it exists
        if(dst!=NULL) {
            rel=rel_type_hash_table_search(rel_type_hash_t,id_rel); // search for the relation type (from its hash table)
            // if exists, check if the relation between the two entities already exists
            if(rel!=NULL) {
                // continue only if the relation does not already exists
                if(source_RB_search(src->source_root,rel,(void *)dst) == &sNIL) {
                    // establish this relation between the two entities
                    add_rel_instance(src,dst,rel);
                }
            }
            else {
                rel=new_rel_type(id_rel); // add the type of relation first
                // establish this relation between the two entities
                add_rel_instance(src,dst,rel);
                rel_type_just_created=FALSE;
            }
        }
    }
}

// deletes an entity
void delent(char *id_ent) {
    //printf("delent \"%s\"\n",ent);
    entity_t *ent=NULL;
    rel_type_t *rel=NULL;
    rel_type_t *next=NULL;
    int i;

    // Check if the entity does exists
    ent=entity_hash_table_search(entity_hash_t,id_ent);
    if(ent!=NULL) {
        /* OPERATIONS ON ENTITY'S STRUCTURES AND DELETION */
        delete_source_relations(&(ent->source_root));
        delete_destination_relations(ent);
        delete_holes(&(ent->rec_root),ent);

        /* ENTITY DELETION */
        entity_hash_table_delete(entity_hash_t,ent); // node is freed by the function
        ent=NULL;

        /* REPORT TREE OPERATIONS */
        // Now, I was forced into throw away the report structure and to create again it because of problems I was unable to resolve
        report_tree=&repNIL; // this is unnecessary (from a semantic view)
        for(i=0;i<REL_TYPE_TABLE_SIZE;i++) {
            if(rel_type_hash_t[i]!=NULL) {
                rel=rel_type_hash_t[i];
                // check the rel_type chained list
                do {
                    // check if the relation type needs to find new maximum(s)
                    //if(rel->max_modified==YES) { ... }
                    rel->maximum=search_maximum(rel); // search for a new maximum hole
                    // if maximum is not zero then add new maximum node(s) to the report tree, else delete the relation type
                    if(rel->maximum != 0) {
                        add_maximum(rel);
                        //rel->max_modified=NO; // reset boolean value
                        next=rel->next;
                    }
                    else {
                        next=rel->next;
                        //rel_type_hash_table_delete(rel_type_hash_t,rel); // rel node is freed by the function
                        rel_type_list_delete(&rel_type_hash_t[i],rel);
                        rel=NULL;
                    }

                    // go to the next node
                    rel=next;
                    next=NULL;
                }while(rel!=NULL);
            }
        }
    }
}

// deletes a relation
void delrel(char *ent_source, char *ent_receiving, char *id_rel) {
    //printf("delrel \"%s\" \"%s\" \"%s\"\n",ent_source,ent_receiving,rel);
    entity_t *src; // source entity
    entity_t *dst; // destination entity
    rel_type_t *rel; // relation type
    source_t *src_node; // source node
    report_node_t *repNode; // report node
    int last_rec_count; // rec_count before it was decremented

    src=entity_hash_table_search(entity_hash_t,ent_source); // search for the source entity
    // continue only if it exists
    if(src!=NULL) {
        dst=entity_hash_table_search(entity_hash_t,ent_receiving); // search for the destination entity
        // continue only if it exists
        if(dst!=NULL) {
            rel=rel_type_hash_table_search(rel_type_hash_t,id_rel); // search for the relation type (from its hash table)
            // if exists, check if the relation between the two entities already exists
            if(rel!=NULL) {
                // continue only if the relation exists
                src_node=source_RB_search(src->source_root,rel,(void *)dst);
                if(src_node != &sNIL) {
                    // delete relation
                    last_rec_count=src_node->ent_hole->rec_count; // save original rec_count counter
                    src_node->ent_hole->rec_count--; // decrement the receiving counter of the relation for the given entity's hole
                    // if rec_count goes down to zero, delete its node
                    if(src_node->ent_hole->rec_count == 0) {
                        receiving_RB_delete(&(dst->rec_root),src_node->ent_hole); // delete node
                        free(src_node->ent_hole); // free node
                        src_node->ent_hole=NULL;
                    }

                    // delete and free source node
                    source_RB_delete(&(src->source_root),src_node);
                    free(src_node);
                    src_node=NULL;

                    /* REPORT TREE OPERATION */
                    // if the destination entity was the maximum for the given relation type, changes may occur
                    if(rel->maximum == last_rec_count) {
                        repNode=report_node_RB_search(report_tree,rel,dst); // search for the node in the report-tree
                        report_node_RB_delete(&report_tree,repNode); // node deletion from the rb
                        free(repNode); // free node
                        repNode=NULL;
                        // search for another node with the same maximum rec_count: if it does not exist, new maximum hole need to be found
                        if(report_node_RB_search_relation(report_tree,rel) == &repNIL) {
                            rel->maximum=search_maximum(rel); // search for a new maximum hole
                            // if maximum is not zero then add new maximum node(s) to the report tree, else delete the relation type
                            if(rel->maximum != 0) {
                                add_maximum(rel);
                            }
                            else {
                                rel_type_hash_table_delete(rel_type_hash_t,rel); // rel node is freed by the function
                                rel=NULL;
                            }
                        }
                        // else: a maximum hole for the given relation type already exists. Do nothing more
                    }
                    // else: the entity was not the maximum for the given relation type. Do nothing
                }
            }
        }
    }

}

// report
void report() {

    if(report_tree != &repNIL) {
        report_node_inorder_tree_walk(report_tree);
        fputc(' ',stdout);
        fputs(lookup[last.num],stdout);
        fputs(";\n",stdout);
        last.id_rel[0]='\0';
        last.num=0;
    }
    else
    {
        fputs("none\n",stdout);
    }
}

rel_type_t* new_rel_type(char *id_rel) {
    rel_type_t *aux;

    // create new node
    aux=(rel_type_t *)malloc(sizeof(rel_type_t));
    aux->next=NULL;
    aux->previous=NULL;
    aux->maximum=0;
    //aux->max_modified=NO;
    aux->id=strdup(id_rel);

    // insert in hash table
    rel_type_hash_table_insert(rel_type_hash_t,&aux);
    rel_type_just_created=TRUE;

    return aux; // return the address of the new relation_type instance
}

// Auxiliary functions
// add anew relation instance between two entities
void add_rel_instance(entity_t *src, entity_t *dst, rel_type_t *rel) {
    source_t *new_source=NULL;
    receiving_t *new_receiving=NULL;
    report_node_t *new_report_node=NULL;
    int max=0;

    // initialize new source node
    new_source=(source_t *)malloc(sizeof(source_t));
    new_source->p=&sNIL;
    new_source->left=&sNIL;
    new_source->right=&sNIL;
    new_source->ent_dest=(void *)dst;
    new_source->rel=rel;
    new_source->ent_hole=NULL;
    new_source->color=RED;

    if(rel_type_just_created == TRUE) {
        // create and initialize a new hole node
        new_receiving=(receiving_t *)malloc(sizeof(receiving_t));
        new_receiving->p=&rNIL;
        new_receiving->left=&rNIL;
        new_receiving->right=&rNIL;
        new_receiving->rel=rel;
        new_receiving->rec_count=1;
        new_receiving->color=RED;

        receiving_RB_insert(&(dst->rec_root),new_receiving); // add new receiving node
    }
    else {
        // search for the hole existence
        new_receiving=receiving_RB_search(dst->rec_root,rel);

        if(new_receiving == &rNIL) {
            // instance a new hole node
            // create and initialize a new hole node
            new_receiving=(receiving_t *)malloc(sizeof(receiving_t));
            new_receiving->p=&rNIL;
            new_receiving->left=&rNIL;
            new_receiving->right=&rNIL;
            new_receiving->rel=rel;
            new_receiving->rec_count=1;
            new_receiving->color=RED;

            receiving_RB_insert(&(dst->rec_root),new_receiving); // add new receiving node
        }
        else {
            // increment the existing hole
            new_receiving->rec_count++;
        }
    }

    new_source->ent_hole=new_receiving;

    source_RB_insert(&(src->source_root),new_source); // add new source node to the RB

    /* REPORT_STRUCTURE operations */
    // check if the receiving entity is the new maximum for the given relation type
    max=rel->maximum;
    if(rel_type_just_created==FALSE && new_receiving->rec_count > max) // if rel_type_just_created != FALSE, max would be NULL
    {
        // set the new maximum
        rel->maximum=new_receiving->rec_count;
        // delete all the relation nodes from the report structure
        report_node_RB_delete_relation(&report_tree,rel);
        // create a new report node
        new_report_node=(report_node_t *)malloc(sizeof(report_node_t));
        new_report_node->p=&repNIL;
        new_report_node->left=&repNIL;
        new_report_node->right=&repNIL;
        new_report_node->rel=rel;
        new_report_node->ent=dst;
        new_report_node->rec_count=new_receiving->rec_count;
        new_report_node->color=RED;
        // modify report structure to handle the new maximum hole
        report_node_RB_insert(&report_tree,new_report_node);
    }
    else if(rel_type_just_created==FALSE && new_receiving->rec_count == max) {
        // create a new report node
        new_report_node=(report_node_t *)malloc(sizeof(report_node_t));
        new_report_node->p=&repNIL;
        new_report_node->left=&repNIL;
        new_report_node->right=&repNIL;
        new_report_node->rel=rel;
        new_report_node->ent=dst;
        new_report_node->rec_count=new_receiving->rec_count;
        new_report_node->color=RED;
        // modify report structure to handle the new maximum hole
        report_node_RB_insert(&report_tree,new_report_node);
    }
    else if(rel_type_just_created == TRUE) {
        // the relation type has just been created, set the maximum
        rel->maximum=new_receiving->rec_count;
        // create a new report node
        new_report_node=(report_node_t *)malloc(sizeof(report_node_t));
        new_report_node->p=&repNIL;
        new_report_node->left=&repNIL;
        new_report_node->right=&repNIL;
        new_report_node->rel=rel;
        new_report_node->ent=dst;
        new_report_node->rec_count=new_receiving->rec_count;
        new_report_node->color=RED;
        // modify report structure to handle the new maximum hole
        report_node_RB_insert(&report_tree,new_report_node);
    }
    // if the hole (new_receiving) is not the new maximum, do nothing
}

// search for the new maximum for a given relation type
int search_maximum(rel_type_t *rel) {
    receiving_t *hole=&rNIL;
    entity_t *ent=NULL;
    int maximum=0;
    int i;

    for(i=0;i<ENTITY_TABLE_SIZE;i++) {
        if(entity_hash_t[i]!=NULL) {
            ent=entity_hash_t[i];
            do {
                hole=receiving_RB_search(ent->rec_root,rel);
                if(hole != &rNIL && hole->rec_count > maximum) {
                    maximum=hole->rec_count;
                }
                ent=ent->next;
            }while(ent!=NULL);
        }
    }

    return maximum;
}

// add new maximum node to the report tree for a given relation type
void add_maximum(rel_type_t *rel) {
    receiving_t *hole=&rNIL;
    report_node_t *new_report_node=NULL;
    entity_t *ent=NULL;
    int i;

    for(i=0;i<ENTITY_TABLE_SIZE;i++) {
        if(entity_hash_t[i]!=NULL) {
            ent=entity_hash_t[i];
            do {
                hole=receiving_RB_search(ent->rec_root,rel);
                if(hole != &rNIL && hole->rec_count == rel->maximum) {
                    /* Add node */
                    // create a new report node
                    new_report_node=(report_node_t *)malloc(sizeof(report_node_t));
                    new_report_node->p=&repNIL;
                    new_report_node->left=&repNIL;
                    new_report_node->right=&repNIL;
                    new_report_node->rel=rel;
                    new_report_node->ent=ent;
                    new_report_node->rec_count=hole->rec_count;
                    new_report_node->color=RED;
                    // modify report structure to handle the new maximum hole
                    report_node_RB_insert(&report_tree,new_report_node);

                    new_report_node=NULL;
                }
                ent=ent->next;
            }while(ent!=NULL);
        }
    }
}

// delete every node (and other operations) in the source tree for a specific entity
void delete_source_relations(source_t **T) {
    report_node_t *repNode=NULL;
    entity_t *dst=NULL;
    int original_rec_count=0;

    if((*T)!=&sNIL) {
        delete_source_relations(&((*T)->left));

        /* NODE OPERATIONS */
        original_rec_count=(*T)->ent_hole->rec_count;
        (*T)->ent_hole->rec_count--;

        // check if the hole was the maximum
        if((*T)->ent_hole->rel->maximum == original_rec_count) {
            // delete report node
            repNode=report_node_RB_search(report_tree,(*T)->rel,(*T)->ent_dest);
            report_node_RB_delete(&report_tree,repNode);
            free(repNode);
            repNode=NULL;

            // check if there already is a new maximum
            /*if(report_node_RB_search_relation(report_tree,(*T)->rel) == &repNIL) {
                (*T)->rel->max_modified=YES;
            }*/
        }

        // if the destination entity's relation hole goes down to zero, delete it
        if((*T)->ent_hole->rec_count == 0) {
            dst=(entity_t *)((*T)->ent_dest);
            receiving_RB_delete(&(dst->rec_root),(*T)->ent_hole);
            free((*T)->ent_hole);
            (*T)->ent_hole=NULL;
        }

        delete_source_relations(&((*T)->right));

        /* NODE ELIMINATION */
        free((*T)); // just free it: no need to properly delete from its RB-tree
        (*T)=&sNIL;
    }
}

// delete every source node in other entity's source-RB-Tree in which the given entity is a destination entity
void delete_destination_relations(entity_t *dst) {
    entity_t *src;
    int i;

    for(i=0;i<ENTITY_TABLE_SIZE;i++) {
        if(entity_hash_t[i]!=NULL) {
            src=entity_hash_t[i];
            // check entities chained list
            do {
                source_RB_delete_entity(&(src->source_root),dst); // every node is freed by the function
                src=src->next;
            }while(src!=NULL);
        }
    }
}

// delete every receiving node form the given entity's hole RB-tree root (recursively)
void delete_holes(receiving_t **T, entity_t *ent) {
    report_node_t *repNode=NULL;

    if((*T)!=&rNIL) {
        delete_holes((&(*T)->left),ent);

        /* NODE OPERATIONS */
        // check if the hole was a maximum for its associated relation type
        if((*T)->rec_count == (*T)->rel->maximum) {
            // if it was, delete the node
            repNode=report_node_RB_search(report_tree,(*T)->rel,ent);
            report_node_RB_delete(&report_tree,repNode);
            free(repNode);
            repNode=NULL;

            // check if a new maximum for that relation type does not exist yet
            /*if(report_node_RB_search_relation(report_tree,(*T)->rel) == &repNIL) {
                // in that case, check the relation type as a max_modified relation
                (*T)->rel->max_modified=YES;
            }*/
        }

        delete_holes((&(*T)->right),ent);

        /* NODE ELIMINATION */
        free((*T)); // just free it: no need to properly delete from its RB-tree
        (*T)=&rNIL;
    }
}

/* FUNCTION DEFINITIONS */
// hash function: returns an array index. NB: This is an implementation for the FNV-1a hash function (good speed and collision avoidance)
unsigned int hf(char *key) {
    register unsigned int hash = FNV_OFFSET_BASIS;
    int i;
    //int i=strlen(key)-1;

    //for each octet of data to be hashed
    //for(;i>=0;i--)
    for(i=0;*(key + i)!='\0';i++) {
        hash ^= *(key + i);
        hash *= FNV_PRIME;
    }

    return hash;
}

// search into the hash table, then return a pointer (undefined type for flexibility)
entity_t* entity_hash_table_search(entity_t *T[], char *key) {
    unsigned int hash=0;
    entity_t *x=NULL;

    hash=hf(key);
    hash%=ENTITY_TABLE_SIZE;
    x=entity_list_search(T[hash],key);

    return x;
}

// insert a node into the hash table
void entity_hash_table_insert(entity_t *T[], entity_t **node) {
    unsigned int hash=0;

    hash=hf((*node)->id);
    hash%=ENTITY_TABLE_SIZE;
    entity_list_insert(&T[hash],node);
}

// delete a node from the hash table
void entity_hash_table_delete(entity_t *T[], entity_t *node) {
    unsigned int hash=0;

    hash=hf(node->id);
    hash%=ENTITY_TABLE_SIZE;
    entity_list_delete(&T[hash],node);
}

// LIST FUNCTIONS (needed because of chained-hashing)
// search into the chained list, then return a pointer (undefined type for flexibility)
entity_t* entity_list_search(entity_t *L, char *key) {

    while(L!=NULL && strcmp(L->id,key)!=0) {
        L=L->next;
    }
    return L;
}

// insert a node into the list
void entity_list_insert(entity_t **L, entity_t **node) {

    (*node)->next=(*L);
    if((*L)!=NULL) {
        (*L)->previous = (*node);
    }
    (*L)=(*node);
    (*L)->previous=NULL;
}

// delete a node from the list
void entity_list_delete(entity_t **L, entity_t *node) {

    if(node->previous!=NULL) {
        node->previous->next=node->next;
    }
    else {
        (*L)=node->next;
    }

    if(node->next!=NULL) {
        node->next->previous=node->previous;
    }

    free(node->id);
    free(node);
    node=NULL;
}

// search into the hash table, then return a pointer (undefined type for flexibility)
rel_type_t* rel_type_hash_table_search(rel_type_t *T[], char *key) {
    unsigned int hash=0;
    rel_type_t *x=NULL;

    hash=hf(key);
    hash%=REL_TYPE_TABLE_SIZE;
    x=rel_type_list_search(T[hash],key);

    return x;
}

// insert a node into the hash table
void rel_type_hash_table_insert(rel_type_t *T[], rel_type_t **node) {
    unsigned int hash=0;

    hash=hf((*node)->id);
    hash%=REL_TYPE_TABLE_SIZE;
    rel_type_list_insert(&T[hash],node);
}

// delete a node from the hash table
void rel_type_hash_table_delete(rel_type_t *T[], rel_type_t *node) {
    unsigned int hash=0;

    hash=hf(node->id);
    hash%=REL_TYPE_TABLE_SIZE;
    rel_type_list_delete(&T[hash],node);
}

// LIST FUNCTIONS (needed because of chained-hashing)
// search into the chained list, then return a pointer (undefined type for flexibility)
rel_type_t* rel_type_list_search(rel_type_t *L, char *key) {

    while(L!=NULL && strcmp(L->id,key)!=0) {
        L=L->next;
    }
    return L;
}

// insert a node into the list
void rel_type_list_insert(rel_type_t **L, rel_type_t **node) {

    (*node)->next=(*L);
    if((*L)!=NULL) {
        (*L)->previous = (*node);
    }
    (*L)=(*node);
    (*L)->previous=NULL;
}

// delete a node from the list
void rel_type_list_delete(rel_type_t **L, rel_type_t *node) {

    if(node->previous!=NULL) {
        node->previous->next=node->next;
    }
    else {
        (*L)=node->next;
    }

    if(node->next!=NULL) {
        node->next->previous=node->previous;
    }

    free(node->id);
    free(node);
    node=NULL;
}

// RB search function
source_t* source_RB_search(source_t *x, rel_type_t *key, void *ent) {

    while(x!=&sNIL && (x->ent_dest!=ent || key!=x->rel)) {

        if(ent < x->ent_dest) {
            x=x->left;
        }
        else if(ent > x->ent_dest) {
            x=x->right;
        }
        else {
            if(key < x->rel) {
                x=x->left;
            }
            else {
                x=x->right;
            }
        }
    }
    return x;
}

// search just for the first relation which has a given entity as its receiving one
/*source_t* source_RB_search_entity(source_t *x, void* ent) {

    while(x!=&sNIL && x->ent_dest!=ent) {

        if(ent < x->ent_dest) {
            x=x->left;
        }
        else {
            x=x->right;
        }
    }
    return x;
}*/

// deletes every relation which has a given entity as its receiving one
void source_RB_delete_entity(source_t **T, void* ent) {
    int done=0; // boolean telling if the deletion has finished
    source_t *x=NULL;

    while(!done) {
        x=(*T);
        while(x!=&sNIL && x->ent_dest!=ent) {

            if(ent < x->ent_dest) {
                x=x->left;
            }
            else {
                x=x->right;
            }
        }

        if(x!=&sNIL) {
            source_RB_delete(T,x);
            free(x);
            x=NULL;
        }
        else {
            done=1; // the deletion has finished
        }
    }
}

// inorder walk
/*void source_inorder_tree_walk(source_t *x) {
    if(x!=&sNIL) {
        source_inorder_tree_walk(x->left);
        printf("Useless print\n");
        source_inorder_tree_walk(x->right);
    }
}*/

// left rotation for adjusting tree
void source_left_rotate(source_t **T, source_t *node) {
    source_t *y;

    y=node->right;
    node->right=y->left;

    if(y->left != &sNIL) {
        y->left->p=node;
    }
    y->p=node->p;
    if(node->p == &sNIL) {
        (*T)=y;
    }
    else if(node == node->p->left) {
        node->p->left=y;
    }
    else {
        node->p->right=y;
    }

    y->left=node;
    node->p=y;
}

// right rotation for adjusting tree
void source_right_rotate(source_t **T, source_t *node) {
    source_t *y;

    y=node->left;
    node->left=y->right;

    if(y->right != &sNIL) {
        y->right->p=node;
    }
    y->p=node->p;
    if(node->p == &sNIL) {
        (*T)=y;
    }
    else if(node == node->p->right) {
        node->p->right=y;
    }
    else {
        node->p->left=y;
    }

    y->right=node;
    node->p=y;
}


// RB insertion functions
void source_RB_insert(source_t **T, source_t *node) {
    source_t *y;
    source_t *x;

    y=&sNIL;
    x=(*T);

    while(x!=&sNIL) {
        y=x;
        if(node->ent_dest < x->ent_dest) {
            x=x->left;
        }
        else if(node->ent_dest > x->ent_dest) {
            x=x->right;
        }
        else {
            // now order for the id_rel
            if(node->rel < x->rel) {
                x=x->left;
            }
            else {
                x=x->right;
            }
        }
    }

    node->p=y;
    if(y==&sNIL) {
        (*T)=node;
    }
    else if(node->ent_dest < y->ent_dest) {
        y->left=node;
    }
    else if(node->ent_dest > y->ent_dest) {
        y->right=node;
    }
    else {
        // now order for the id_rel
        if(node->rel < y->rel) {
            y->left=node;
        }
        else {
            y->right=node;
        }
    }

    node->left=&sNIL;
    node->right=&sNIL;
    node->color=RED;
    source_RB_insert_fixup(T,node);
}

void source_RB_insert_fixup(source_t **T, source_t *node) {
    source_t *y=&sNIL;

    while((*T)!=node && node->p->color == RED) {
        if(node->p == node->p->p->left) {
            y=node->p->p->right;
            if(y && y->color == RED) {
                node->p->color=BLACK;
                y->color=BLACK;
                node->p->p->color=RED;
                node=node->p->p;
            }
            else {
                if(node == node->p->right) {
                    node=node->p;
                    source_left_rotate(T,node);
                }
                node->p->color=BLACK;
                node->p->p->color=RED;
                source_right_rotate(T,node->p->p);
            }
        }
        else {
            // (come la clausola then con "right" e "left" scambiati)
            y=node->p->p->left;
            if(y && y->color == RED) {
                node->p->color=BLACK;
                y->color=BLACK;
                node->p->p->color=RED;
                node=node->p->p;
            }
            else {
                if(node == node->p->left) {
                    node=node->p;
                    source_right_rotate(T,node);
                }
                node->p->color=BLACK;
                node->p->p->color=RED;
                source_left_rotate(T,node->p->p);
            }
        }
    }

    (*T)->color=BLACK;
}

// RB deletion function
source_t* source_tree_minimum(source_t *node) {
    while(node->left != &sNIL) {
        node=node->left;
    }
    return node;
}

void source_RB_transplant(source_t **T, source_t *node1, source_t *node2) {
    if(node1->p == &sNIL) {
        (*T)=node2;
    }
    else if(node1 == node1->p->left) {
        node1->p->left=node2;
    }
    else {
        node1->p->right=node2;
    }
    node2->p=node1->p;
}

void source_RB_delete(source_t **T, source_t *node) {
    source_t *y=&sNIL;
    source_t *x=&sNIL;
    char y_original_color;

    y=node;
    y_original_color=y->color;
    if(node->left == &sNIL) {
        x=node->right;
        source_RB_transplant(T,node,node->right);
    }
    else if(node->right == &sNIL) {
        x=node->left;
        source_RB_transplant(T,node,node->left);
    }
    else {
        y=source_tree_minimum(node->right);
        y_original_color=y->color;
        x=y->right;
        if(y->p == node) {
            x->p=y;
        }
        else {
            source_RB_transplant(T,y,y->right);
            y->right=node->right;
            y->right->p=y;
        }
        source_RB_transplant(T,node,y);
        y->left=node->left;
        y->left->p=y;
        y->color=node->color;
    }

    if(y_original_color == BLACK) {
        source_RB_delete_fixup(T,x);
    }
}

void source_RB_delete_fixup(source_t **T, source_t *node) {
    source_t *w=&sNIL;

    while(node!=(*T) && node->color==BLACK) {
        if(node == node->p->left) {
            w=node->p->right;
            if(w->color == RED) {
                w->color=BLACK;
                node->p->color=RED;
                source_left_rotate(T,node->p);
                w=node->p->right;
            }
            if(w->left->color==BLACK && w->right->color==BLACK) {
                w->color=RED;
                node=node->p;
            }
            else {
                if(w->right->color == BLACK) {
                    w->left->color=BLACK;
                    w->color=RED;
                    source_right_rotate(T,w);
                    w=node->p->right;
                }
                w->color=node->p->color;
                node->p->color=BLACK;
                w->right->color=BLACK;
                source_left_rotate(T,node->p);
                node=(*T);
            }
        }
        else {
            // (come la clausola then con "right" e "left" scambiati)
            w=node->p->left;
            if(w->color == RED) {
                w->color=BLACK;
                node->p->color=RED;
                source_right_rotate(T,node->p);
                w=node->p->left;
            }
            if(w->right->color==BLACK && w->left->color==BLACK) {
                w->color=RED;
                node=node->p;
            }
            else {
                if(w->left->color == BLACK) {
                    w->right->color=BLACK;
                    w->color=RED;
                    source_left_rotate(T,w);
                    w=node->p->left;
                }
                w->color=node->p->color;
                node->p->color=BLACK;
                w->left->color=BLACK;
                source_right_rotate(T,node->p);
                node=(*T);
            }
        }
    }

    node->color=BLACK;
}

// RB search function
receiving_t* receiving_RB_search(receiving_t *x, rel_type_t *key) {

    while(x!=&rNIL && key!=x->rel) {

        if(key < x->rel) {
            x=x->left;
        }
        else {
            x=x->right;
        }
    }
    return x;
}

// inorder walk
/*void receiving_inorder_tree_walk(receiving_t *x) {
    if(x!=&rNIL) {
        receiving_inorder_tree_walk(x->left);
        printf("Useless print");
        receiving_inorder_tree_walk(x->right);
    }
}*/

// left rotation for adjusting tree
void receiving_left_rotate(receiving_t **T, receiving_t *node) {
    receiving_t *y;

    y=node->right;
    node->right=y->left;

    if(y->left != &rNIL) {
        y->left->p=node;
    }
    y->p=node->p;
    if(node->p == &rNIL) {
        (*T)=y;
    }
    else if(node == node->p->left) {
        node->p->left=y;
    }
    else {
        node->p->right=y;
    }

    y->left=node;
    node->p=y;
}

// right rotation for adjusting tree
void receiving_right_rotate(receiving_t **T, receiving_t *node) {
    receiving_t *y;

    y=node->left;
    node->left=y->right;

    if(y->right != &rNIL) {
        y->right->p=node;
    }
    y->p=node->p;
    if(node->p == &rNIL) {
        (*T)=y;
    }
    else if(node == node->p->right) {
        node->p->right=y;
    }
    else {
        node->p->left=y;
    }

    y->right=node;
    node->p=y;
}


// RB insertion functions
void receiving_RB_insert(receiving_t **T, receiving_t *node) {
    receiving_t *y;
    receiving_t *x;

    y=&rNIL;
    x=(*T);

    while(x!=&rNIL) {
        y=x;
        if(node->rel < x->rel) {
            x=x->left;
        }
        else {
            x=x->right;
        }
    }

    node->p=y;
    if(y==&rNIL) {
        (*T)=node;
    }
    else if(node->rel < y->rel) {
        y->left=node;
    }
    else {
        y->right=node;
    }

    node->left=&rNIL;
    node->right=&rNIL;
    node->color=RED;
    receiving_RB_insert_fixup(T,node);
}

void receiving_RB_insert_fixup(receiving_t **T, receiving_t *node) {
    receiving_t *y=&rNIL;

    while((*T)!=node && node->p->color == RED) {
        if(node->p == node->p->p->left) {
            y=node->p->p->right;
            if(y && y->color == RED) {
                node->p->color=BLACK;
                y->color=BLACK;
                node->p->p->color=RED;
                node=node->p->p;
            }
            else {
                if(node == node->p->right) {
                    node=node->p;
                    receiving_left_rotate(T,node);
                }
                node->p->color=BLACK;
                node->p->p->color=RED;
                receiving_right_rotate(T,node->p->p);
            }
        }
        else {
            // (come la clausola then con "right" e "left" scambiati)
            y=node->p->p->left;
            if(y && y->color == RED) {
                node->p->color=BLACK;
                y->color=BLACK;
                node->p->p->color=RED;
                node=node->p->p;
            }
            else {
                if(node == node->p->left) {
                    node=node->p;
                    receiving_right_rotate(T,node);
                }
                node->p->color=BLACK;
                node->p->p->color=RED;
                receiving_left_rotate(T,node->p->p);
            }
        }
    }

    (*T)->color=BLACK;
}

// RB deletion function
receiving_t* receiving_tree_minimum(receiving_t *node) {
    while(node->left != &rNIL) {
        node=node->left;
    }
    return node;
}

void receiving_RB_transplant(receiving_t **T, receiving_t *node1, receiving_t *node2) {
    if(node1->p == &rNIL) {
        (*T)=node2;
    }
    else if(node1 == node1->p->left) {
        node1->p->left=node2;
    }
    else {
        node1->p->right=node2;
    }
    node2->p=node1->p;
}

void receiving_RB_delete(receiving_t **T, receiving_t *node) {
    receiving_t *y=&rNIL;
    receiving_t *x=&rNIL;
    char y_original_color;

    y=node;
    y_original_color=y->color;
    if(node->left == &rNIL) {
        x=node->right;
        receiving_RB_transplant(T,node,node->right);
    }
    else if(node->right == &rNIL) {
        x=node->left;
        receiving_RB_transplant(T,node,node->left);
    }
    else {
        y=receiving_tree_minimum(node->right);
        y_original_color=y->color;
        x=y->right;
        if(y->p == node) {
            x->p=y;
        }
        else {
            receiving_RB_transplant(T,y,y->right);
            y->right=node->right;
            y->right->p=y;
        }
        receiving_RB_transplant(T,node,y);
        y->left=node->left;
        y->left->p=y;
        y->color=node->color;
    }

    if(y_original_color == BLACK) {
        receiving_RB_delete_fixup(T,x);
    }
}

void receiving_RB_delete_fixup(receiving_t **T, receiving_t *node) {
    receiving_t *w=&rNIL;

    while(node!=(*T) && node->color==BLACK) {
        if(node == node->p->left) {
            w=node->p->right;
            if(w->color == RED) {
                w->color=BLACK;
                node->p->color=RED;
                receiving_left_rotate(T,node->p);
                w=node->p->right;
            }
            if(w->left->color==BLACK && w->right->color==BLACK) {
                w->color=RED;
                node=node->p;
            }
            else {
                if(w->right->color == BLACK) {
                    w->left->color=BLACK;
                    w->color=RED;
                    receiving_right_rotate(T,w);
                    w=node->p->right;
                }
                w->color=node->p->color;
                node->p->color=BLACK;
                w->right->color=BLACK;
                receiving_left_rotate(T,node->p);
                node=(*T);
            }
        }
        else {
            // (come la clausola then con "right" e "left" scambiati)
            w=node->p->left;
            if(w->color == RED) {
                w->color=BLACK;
                node->p->color=RED;
                receiving_right_rotate(T,node->p);
                w=node->p->left;
            }
            if(w->right->color==BLACK && w->left->color==BLACK) {
                w->color=RED;
                node=node->p;
            }
            else {
                if(w->left->color == BLACK) {
                    w->right->color=BLACK;
                    w->color=RED;
                    receiving_left_rotate(T,w);
                    w=node->p->left;
                }
                w->color=node->p->color;
                node->p->color=BLACK;
                w->left->color=BLACK;
                receiving_right_rotate(T,node->p);
                node=(*T);
            }
        }
    }

    node->color=BLACK;
}

// RB search function
report_node_t* report_node_RB_search(report_node_t *x, rel_type_t *rel, entity_t *ent) {
    int resultRel=0;
    int resultEnt=0;

    while(x!=&repNIL && ((resultRel=strcmp(rel->id,x->rel->id))!=0 || (resultEnt=strcmp(ent->id,x->ent->id))!=0)) {

        if(resultRel < 0) {
            x=x->left;
        }
        else if(resultRel > 0) {
            x=x->right;
        }
        else {
            if(resultEnt < 0) {
                x=x->left;
            }
            else {
                x=x->right;
            }
        }
    }
    return x;
}

// search for any node which is associated with a given relation type
report_node_t* report_node_RB_search_relation(report_node_t *x, rel_type_t *rel) {
    int result=0;

    while(x!=&repNIL && (result=strcmp(rel->id,x->rel->id))!=0) {

        if(result < 0) {
            x=x->left;
        }
        else {
            x=x->right;
        }
    }
    return x;
}

// deletes every relation which has a given relation_type
void report_node_RB_delete_relation(report_node_t **T, rel_type_t* rel) {
    int done=0; // boolean telling if the deletion has finished
    int result=0;
    report_node_t *x=NULL;

    while(!done) {
        x=(*T);
        while(x!=&repNIL && (result=strcmp(rel->id,x->rel->id))!=0) {

            if(result < 0) {
                x=x->left;
            }
            else {
                x=x->right;
            }
        }

        if(x!=&repNIL) {
            report_node_RB_delete(T,x);
            free(x);
            x=NULL;
        }
        else {
            done=1; // the deletion has finished
        }
    }
}

// inorder walk
void report_node_inorder_tree_walk(report_node_t *x) {
    if(x!=&repNIL) {
        report_node_inorder_tree_walk(x->left);


        if(strcmp(x->rel->id,last.id_rel) != 0) {
            if(last.id_rel[0] != '\0') {
                // starting stage passed: concatenate the number
                fputc(' ',stdout);
                fputs(lookup[last.num],stdout);
                fputs("; ",stdout);
            }
            fputc('"',stdout);
            fputs(x->rel->id,stdout);
            fputc('"',stdout);

            // save the last relation state
            strcpy(last.id_rel,x->rel->id);
            last.num=x->rec_count;
        }
        fputs(" \"",stdout);
        fputs(x->ent->id,stdout);
        fputs("\"",stdout);


        report_node_inorder_tree_walk(x->right);
    }
}

// left rotation for adjusting tree
void report_node_left_rotate(report_node_t **T, report_node_t *node) {
    report_node_t *y;

    y=node->right;
    node->right=y->left;

    if(y->left != &repNIL) {
        y->left->p=node;
    }
    y->p=node->p;
    if(node->p == &repNIL) {
        (*T)=y;
    }
    else if(node == node->p->left) {
        node->p->left=y;
    }
    else {
        node->p->right=y;
    }

    y->left=node;
    node->p=y;
}

// right rotation for adjusting tree
void report_node_right_rotate(report_node_t **T, report_node_t *node) {
    report_node_t *y;

    y=node->left;
    node->left=y->right;

    if(y->right != &repNIL) {
        y->right->p=node;
    }
    y->p=node->p;
    if(node->p == &repNIL) {
        (*T)=y;
    }
    else if(node == node->p->right) {
        node->p->right=y;
    }
    else {
        node->p->left=y;
    }

    y->right=node;
    node->p=y;
}


// RB insertion functions
void report_node_RB_insert(report_node_t **T, report_node_t *node) {
    report_node_t *y;
    report_node_t *x;
    int result;

    y=&repNIL;
    x=(*T);

    while(x!=&repNIL) {
        y=x;

        result=strcmp(node->rel->id,x->rel->id);
        if(result < 0) {
            x=x->left;
        }
        else if(result > 0) {
            x=x->right;
        }
        else {
            // now order for the ent
            if(strcmp(node->ent->id,x->ent->id) < 0) {
                x=x->left;
            }
            else {
                x=x->right;
            }
        }
    }

    node->p=y;
    if(y==&repNIL) {
        (*T)=node;
    }
    else if(result < 0) {
        y->left=node;
    }
    else if(result > 0) {
        y->right=node;
    }
    else {
        // now order for the ent
        if(strcmp(node->ent->id,y->ent->id) < 0) {
            y->left=node;
        }
        else {
            y->right=node;
        }
    }

    node->left=&repNIL;
    node->right=&repNIL;
    node->color=RED;
    report_node_RB_insert_fixup(T,node);
}

void report_node_RB_insert_fixup(report_node_t **T, report_node_t *node) {
    report_node_t *y=&repNIL;

    while((*T)!=node && node->p->color == RED) {
        if(node->p == node->p->p->left) {
            y=node->p->p->right;
            if(y && y->color == RED) {
                node->p->color=BLACK;
                y->color=BLACK;
                node->p->p->color=RED;
                node=node->p->p;
            }
            else {
                if(node == node->p->right) {
                    node=node->p;
                    report_node_left_rotate(T,node);
                }
                node->p->color=BLACK;
                node->p->p->color=RED;
                report_node_right_rotate(T,node->p->p);
            }
        }
        else {
            // (come la clausola then con "right" e "left" scambiati)
            y=node->p->p->left;
            if(y && y->color == RED) {
                node->p->color=BLACK;
                y->color=BLACK;
                node->p->p->color=RED;
                node=node->p->p;
            }
            else {
                if(node == node->p->left) {
                    node=node->p;
                    report_node_right_rotate(T,node);
                }
                node->p->color=BLACK;
                node->p->p->color=RED;
                report_node_left_rotate(T,node->p->p);
            }
        }
    }

    (*T)->color=BLACK;
}

// RB deletion function
report_node_t* report_node_tree_minimum(report_node_t *node) {
    while(node->left != &repNIL) {
        node=node->left;
    }
    return node;
}

void report_node_RB_transplant(report_node_t **T, report_node_t *node1, report_node_t *node2) {
    if(node1->p == &repNIL) {
        (*T)=node2;
    }
    else if(node1 == node1->p->left) {
        node1->p->left=node2;
    }
    else {
        node1->p->right=node2;
    }
    node2->p=node1->p;
}

void report_node_RB_delete(report_node_t **T, report_node_t *node) {
    report_node_t *y=&repNIL;
    report_node_t *x=&repNIL;
    char y_original_color;

    y=node;
    y_original_color=y->color;
    if(node->left == &repNIL) {
        x=node->right;
        report_node_RB_transplant(T,node,node->right);
    }
    else if(node->right == &repNIL) {
        x=node->left;
        report_node_RB_transplant(T,node,node->left);
    }
    else {
        y=report_node_tree_minimum(node->right);
        y_original_color=y->color;
        x=y->right;
        if(y->p == node) {
            x->p=y;
        }
        else {
            report_node_RB_transplant(T,y,y->right);
            y->right=node->right;
            y->right->p=y;
        }
        report_node_RB_transplant(T,node,y);
        y->left=node->left;
        y->left->p=y;
        y->color=node->color;
    }

    if(y_original_color == BLACK) {
        report_node_RB_delete_fixup(T,x);
    }
}

void report_node_RB_delete_fixup(report_node_t **T, report_node_t *node) {
    report_node_t *w=&repNIL;

    while(node!=(*T) && node->color==BLACK) {
        if(node == node->p->left) {
            w=node->p->right;
            if(w->color == RED) {
                w->color=BLACK;
                node->p->color=RED;
                report_node_left_rotate(T,node->p);
                w=node->p->right;
            }
            if(w->left->color==BLACK && w->right->color==BLACK) {
                w->color=RED;
                node=node->p;
            }
            else {
                if(w->right->color == BLACK) {
                    w->left->color=BLACK;
                    w->color=RED;
                    report_node_right_rotate(T,w);
                    w=node->p->right;
                }
                w->color=node->p->color;
                node->p->color=BLACK;
                w->right->color=BLACK;
                report_node_left_rotate(T,node->p);
                node=(*T);
            }
        }
        else {
            // (come la clausola then con "right" e "left" scambiati)
            w=node->p->left;
            if(w->color == RED) {
                w->color=BLACK;
                node->p->color=RED;
                report_node_right_rotate(T,node->p);
                w=node->p->left;
            }
            if(w->right->color==BLACK && w->left->color==BLACK) {
                w->color=RED;
                node=node->p;
            }
            else {
                if(w->left->color == BLACK) {
                    w->right->color=BLACK;
                    w->color=RED;
                    report_node_left_rotate(T,w);
                    w=node->p->left;
                }
                w->color=node->p->color;
                node->p->color=BLACK;
                w->left->color=BLACK;
                report_node_right_rotate(T,node->p);
                node=(*T);
            }
        }
    }

    node->color=BLACK;
}
