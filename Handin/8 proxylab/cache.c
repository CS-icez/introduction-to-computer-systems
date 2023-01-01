/**
 * @file    proxy.c
 * @author  icez_BaiTianYu 2100013011@stu.pku.edu.cn
 * @brief   proxylab cache implementation
 * @date    2022.12.21 - 2022.12.22
 * @note    In this file I never checked the return value of pthread lock operations.
 *          It is unsafe, but enough to pass all the tests.
 *          Anyway, this tiny cache cannot be used for real purposes.
 */

#include "cache.h"

struct _cache{
    pthread_rwlock_t lock;      /* Rwlock on the whole struct object. */
    int timer;                  /* The bigger, the more recent used. 0 denotes empty. */
    char url[MAXLINE];          /* Content URL. */
    char content[CONTENT_SIZE]; /* Content. */
    size_t length;              /* Length of content in terms of bytes. */
} *cache = NULL;
int timer = 0;                  /* Global timer. */
pthread_mutex_t timer_mutex;    /* Mutex on timer. */

int cache_init(void){
    cache = calloc(CACHE_SIZE, sizeof(*cache));
    if (!cache) return -1;

    for (int i = 0; i < CACHE_SIZE; ++i)
        pthread_rwlock_init(&cache[i].lock, NULL);

    pthread_mutex_init(&timer_mutex, NULL) < 0;

    return 0;
}

int cache_request(char url[MAXLINE], char buf[CONTENT_SIZE]){
    int length = -1;

    for (int i = 0; i < CACHE_SIZE && length == -1; ++i){
        pthread_rwlock_rdlock(&cache[i].lock);

        if (!strcmp(url, cache[i].url)){
            pthread_mutex_lock(&timer_mutex);
            ++timer;
            pthread_mutex_unlock(&timer_mutex);

            cache[i].timer = timer;
            memcpy(buf, cache[i].content, cache[i].length);
            length = cache[i].length;
        }

        pthread_rwlock_unlock(&cache[i].lock);   
    }

    return length;
}

void cache_record(char url[MAXLINE], char buf[CONTENT_SIZE], size_t length){
    int idx = -1, min_timer = 0x3fffffff, has_empty = 0;

    for (int i = 0; i < CACHE_SIZE && !has_empty; ++i){
        pthread_rwlock_rdlock(&cache[i].lock);

        if (!cache[i].timer){
            idx = i, has_empty = 1;
        }
        else if (cache[i].timer < min_timer){
            idx = i, min_timer = cache[i].timer;
        }

        pthread_rwlock_unlock(&cache[i].lock);
    }

    pthread_mutex_lock(&timer_mutex);
    ++timer;
    pthread_mutex_unlock(&timer_mutex);

    pthread_rwlock_wrlock(&cache[idx].lock);
    cache[idx].timer = timer;
    strcpy(cache[idx].url, url);
    memcpy(cache[idx].content, buf, length);
    cache[idx].length = length;
    pthread_rwlock_unlock(&cache[idx].lock);
}

void cache_destroy(void){
    for (int i = 0; i < CACHE_SIZE; ++i)
        pthread_rwlock_destroy(&cache[i].lock);
    pthread_mutex_destroy(&timer_mutex);
}