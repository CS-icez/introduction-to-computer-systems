// @author icez_BaiTianYu 2100013011@stu.pku.edu.cn

/*
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */
#include <stdio.h>
#include "cachelab.h"
#include "contracts.h"

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/*
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded. The REQUIRES and ENSURES from 15-122 are included
 *     for your convenience. They can be removed if you like.
 */
char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    REQUIRES(M > 0);
    REQUIRES(N > 0);

    int t0, t1, t2, t3, t4, t5, t6, t7, i, j, k, l;

    if (M == 32 && N == 32){
        /*
        Solution for 32*32 --- achieved theoretical miss limit of 1/8
        Core idea: 
            1. divide the matrix into blocks of 8*8
            2. use 8 local variables as a minor cache
            3. copy then transpose, not at a time
        */
        for (i = 0; i < 32; i += 8)
            for (j = 0; j < 32; j += 8){
                for (k = 0; k < 8; ++k){
                    t0 = A[i+k][j+0]; t1 = A[i+k][j+1]; t2 = A[i+k][j+2]; t3 = A[i+k][j+3];
                    t4 = A[i+k][j+4]; t5 = A[i+k][j+5]; t6 = A[i+k][j+6]; t7 = A[i+k][j+7];
                    
                    B[j+k][i+0] = t0; B[j+k][i+1] = t1; B[j+k][i+2] = t2; B[j+k][i+3] = t3;
                    B[j+k][i+4] = t4; B[j+k][i+5] = t5; B[j+k][i+6] = t6; B[j+k][i+7] = t7;
                }
                for (k = 0; k < 8; ++k)
                    for (l = 0; l < k; ++l){
                        t0 = B[j+k][i+l];
                        B[j+k][i+l] = B[j+l][i+k];
                        B[j+l][i+k] = t0;
                    }   
            }

    }

    else if (M == 64 && N == 64){
        /*
        Solution for 64*64 --- achieved theoretical miss limit of 1/8
        Core idea:
            1. divide the matrix into blocks of 8*8, each of which is devided again into blocks of 4*4
            2. use 8 local variables as a minor cache
            3. use the next 8*8 block as a minor cache
            (I learned this idea from https://zhuanlan.zhihu.com/p/387662272. I'm not ashamed to admit that.)
            4. copy to the "wrong" place, maybe even not transposed, but eventally to where it belongs, all for a lower miss rate
        */
        for (j = 0; j < 64; j += 8){
            i = j? 0: 8;

            for (k = 0; k < 4; ++k){
                t0 = A[j+k+4][j+0]; t1 = A[j+k+4][j+1]; t2 = A[j+k+4][j+2]; t3 = A[j+k+4][j+3];
                t4 = A[j+k+4][j+4]; t5 = A[j+k+4][j+5]; t6 = A[j+k+4][j+6]; t7 = A[j+k+4][j+7];

                B[j+k][i+0] = t0; B[j+k][i+1] = t1; B[j+k][i+2] = t2; B[j+k][i+3] = t3;
                B[j+k][i+4] = t4; B[j+k][i+5] = t5; B[j+k][i+6] = t6; B[j+k][i+7] = t7;
            }

            for (k = 0; k < 4; ++k){
                t0 = A[j+k][j+0]; t1 = A[j+k][j+1]; t2 = A[j+k][j+2]; t3 = A[j+k][j+3];
                t4 = A[j+k][j+4]; t5 = A[j+k][j+5]; t6 = A[j+k][j+6]; t7 = A[j+k][j+7];

                B[j+k][j+0] = t0; B[j+k][j+1] = t1; B[j+k][j+2] = t2; B[j+k][j+3] = t3;
                B[j+k][j+4] = t4; B[j+k][j+5] = t5; B[j+k][j+6] = t6; B[j+k][j+7] = t7;
            }

            for (k = 0; k < 4; ++k)
                for (l = 0; l < k; ++l){
                    t0 = B[j+k][j+l], B[j+k][j+l] = B[j+l][j+k], B[j+l][j+k] = t0;

                    t0 = B[j+k][j+l+4], B[j+k][j+l+4] = B[j+l][j+k+4], B[j+l][j+k+4] = t0;
                
                    t0 = B[j+k][i+l], B[j+k][i+l] = B[j+l][i+k], B[j+l][i+k] = t0;

                    t0 = B[j+k][i+l+4], B[j+k][i+l+4] = B[j+l][i+k+4], B[j+l][i+k+4] = t0;
                }
            
            for (k = 0; k < 4; ++k)
                for (l = 0; l < 4; ++l){
                    t0 = B[j+k][j+l+4];
                    B[j+k][j+l+4] = B[j+k][i+l];
                    B[j+k][i+l] = t0;
                }

            for (k = 0; k < 4; ++k)
                for (l = 0; l < 8; ++l)
                    B[j+k+4][j+l] = B[j+k][i+l];        

            for (i = 0; i < 64; i += 8){
                if (i == j) continue;
                for (k = 0; k < 4; ++k){
                    t0 = A[i+k][j+0]; t1 = A[i+k][j+1]; t2 = A[i+k][j+2]; t3 = A[i+k][j+3];
                    t4 = A[i+k][j+4]; t5 = A[i+k][j+5]; t6 = A[i+k][j+6]; t7 = A[i+k][j+7];
                    
                    B[j+0][i+k] = t0; B[j+1][i+k] = t1; B[j+2][i+k] = t2; B[j+3][i+k] = t3; 
                    B[j+0][i+k+4] = t4; B[j+1][i+k+4] = t5; B[j+2][i+k+4] = t6; B[j+3][i+k+4] = t7;
                }

                for (k = 0; k < 4; ++k){
                    t0 = A[i+4][j+k]; t1 = A[i+5][j+k]; t2 = A[i+6][j+k]; t3 = A[i+7][j+k];
                    
                    t4 = A[i+4][j+k+4]; t5 = A[i+5][j+k+4]; t6 = A[i+6][j+k+4]; t7 = A[i+7][j+k+4]; 

                    l = B[j+k][i+4], B[j+k][i+4] = t0, t0 = l;
                    l = B[j+k][i+5], B[j+k][i+5] = t1, t1 = l;
                    l = B[j+k][i+6], B[j+k][i+6] = t2, t2 = l;
                    l = B[j+k][i+7], B[j+k][i+7] = t3, t3 = l;

                    B[j+k+4][i+0] = t0; B[j+k+4][i+1] = t1; B[j+k+4][i+2] = t2; B[j+k+4][i+3] = t3;
                    B[j+k+4][i+4] = t4; B[j+k+4][i+5] = t5; B[j+k+4][i+6] = t6; B[j+k+4][i+7] = t7;
                }
            }
        }
    }

    else if (M == 60 && N == 68){
        /*
        Solution for 60*68 --- cache is magic
        I enumerated many block sizes and iterating orders. The following is the best one I've found, with misses 1477.
        */
        for (i = 0; i < 64; i += 8)
            for (j = 0; j < 56; j += 8)
                for (k = 0; k < 8; ++k){
                    t0 = A[i+0][j+k]; t1 = A[i+1][j+k]; t2 = A[i+2][j+k]; t3 = A[i+3][j+k]; 
                    t4 = A[i+4][j+k]; t5 = A[i+5][j+k]; t6 = A[i+6][j+k]; t7 = A[i+7][j+k];

                    B[j+k][i+0] = t0; B[j+k][i+1] = t1; B[j+k][i+2] = t2; B[j+k][i+3] = t3;
                    B[j+k][i+4] = t4; B[j+k][i+5] = t5; B[j+k][i+6] = t6; B[j+k][i+7] = t7;
                }

        for (j = 0; j < 60; ++j){
            t0 = A[64][j]; t1 = A[65][j]; t2 = A[66][j]; t3 = A[67][j];
            B[j][64] = t0; B[j][65] = t1; B[j][66] = t2; B[j][67] = t3; 
        }

        for (i = 0; i < 64; ++i){
            t0 = A[i][56]; t1 = A[i][57]; t2 = A[i][58]; t3 = A[i][59];
            B[56][i] = t0; B[57][i] = t1; B[58][i] = t2; B[59][i] = t3; 
        }
    }

    ENSURES(is_transpose(M, N, A, B));
}

/*
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started.
 */

 /*
  * trans - A simple baseline transpose function, not optimized for the cache.
  */
char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, tmp;

    REQUIRES(M > 0);
    REQUIRES(N > 0);

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; j++) {
            tmp = A[i][j];
            B[j][i] = tmp;
        }
    }

    ENSURES(is_transpose(M, N, A, B));
}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc);

    /* Register any additional transpose functions */
    registerTransFunction(trans, trans_desc);

}

/*
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

