// @author icez_BaiTianYu 2100013011@stu.pku.edu.cn

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <unistd.h>

void printSummary(int hits,  /* number of hits */
                  int misses, /* number of misses */
                  int evictions); /* number of evictions */

int s = -1, S = -1, E = -1, b = -1, t = -1; // cache arguments
typedef struct{
    int valid;  // 0 denotes invalid, non-0 denotes valid, the bigger, the more recently used
    long tag;   // cache tag
} CacheLine;    // cache line
CacheLine* cache;   // pointer to the entire cache

const char* traceFile;    // trace file path
char line[55];  // the trace file line to be handled
int verbosity;  // verbose flag. I didn't implement this feature

int hit, miss, eviction;   // correspending counter
int cnt;    // global timer

// get tag or set index of an address
long getT(long x){
    return x >> (s+b);
}
long getS(unsigned long x){
    return (x<<t)>>(t+b);
}

// core function, update cache
void updateCache(size_t adr){ // accessed address 
    long ss = getS(adr), tt = getT(adr);    // set and tag index of adr
    int ex = -1;   // target line
    CacheLine* p = cache+ss*E;  // points to set ss

    for (int i = 0; i < E; ++i){
        if (p[i].valid && p[i].tag == tt){  // hit
            ex = i;
            break;
        }
    }

    // hit
    if (ex >= 0){
        p[ex].valid = ++cnt;
        ++hit;
        return;
    }
    
    // miss
    ++miss;
    ex = 0;
    for (int i = 0; i < E; ++i){
        if (!p[i].valid){   // unused line
            ex = i;
            break;
        }
        if (p[i].valid < p[ex].valid)   // less recently used line
            ex = i;
    }
    
    if (p[ex].valid) ++eviction;    // block evicted
    p[ex].valid = ++cnt, p[ex].tag = tt;    // update this line
}  

void printUsage(char* fileName){
    printf("Usage: %s [-hv] -s <num> -E <num> -b <num> -t <file>", fileName);
    puts("Options:");
    puts("  -h         Print this help message.");
    puts("  -v         Optional verbose flag.");
    puts("  -s <num>   Number of set index bits.");
    puts("  -E <num>   Number of lines per set.");
    puts("  -b <num>   Number of block offset bits.");
    puts("  -t <file>  Trace file.");
    puts("Examples:");
    puts("  linux>  ./csim-ref -s 4 -E 1 -b 4 -t traces/yi.trace");
    puts("  linux>  ./csim-ref -v -s 8 -E 2 -b 4 -t traces/yi.trace");
}

int main(int argc, char** argv){
    // handle cmd arguments
    int opt;
    while ((opt = getopt(argc, argv, "s:E:b:t:vh")) != -1){
        switch (opt){
            case 's': sscanf(optarg, "%d", &s); break;
            case 'E': sscanf(optarg, "%d", &E); break;
            case 'b': sscanf(optarg, "%d", &b); break;
            case 't': traceFile = optarg; break;
            case 'h': printUsage(argv[0]); exit(EXIT_SUCCESS);
            case 'v': verbosity = 1; break;
            case '?': printUsage(argv[0]); exit(EXIT_FAILURE);
            default: exit(EXIT_FAILURE);
        }
    }

    if (s < 0 || E <= 0 || b < 0 || s+b > 64 || !freopen(traceFile, "r", stdin)){
        fprintf(stderr, "Command line arguments error.\n");
        printUsage(argv[0]);
        exit(EXIT_FAILURE);
    }

    // calculate cache arguments, create cache
    S = 1 << s;
    t = 64-b-s;
    cache = calloc(S*E, sizeof(CacheLine));
    if (!cache){
        fprintf(stderr, "Bad alloc error.\n");
        exit(EXIT_FAILURE);
    }

    size_t adr;   // accessed address
    while (fgets(line, 50, stdin)){ // assume the file is well-formatted
        if (line[0] != ' ') continue;   // ignore I
        sscanf(line+3, "%lx", &adr);
        updateCache(adr); // L, S, M are basically the same in our simulator
        if (line[1] == 'M') ++hit;
    }

    printSummary(hit, miss, eviction);
    free(cache);
}