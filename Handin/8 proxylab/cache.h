/**
 * @file    cache.c
 * @author  icez_BaiTianYu 2100013011@stu.pku.edu.cn
 * @brief   proxylab cache header
 * @date    2022.12.21 - 2022.12.22
 * @note    This is an over-simplified web cache which cannot be used in any real systems.
 *          Yet, it supports concurrent visit and performs LRU eviction policy.
 */

#include "csapp.h"

/* Cache content size */
#define CONTENT_SIZE 102400
/* Cache size */
#define CACHE_SIZE   10

/**
 * @brief   Initialize web cache.
 * @return  0 on success, -1 on error.
*/
int cache_init(void);

/**
 * @brief   Request content from web cache.
 * @param   url Request URL, used as key to cache content.
 * @param   buf Destination buffer.
 * @return  Size of content if finds, -1 otherwise.
 * @note    Different URLs may actually point to the same content. 
 *          But we don't take that into consideration.
*/
int cache_request(char url[MAXLINE], char buf[CONTENT_SIZE]);

/**
 * @brief   Record content into web cache.
 * @param   url     URL to content.
 * @param   buf     Buffer holding content.
 * @param   length  Length of content in terms of bytes.
 * 
*/
void cache_record(char url[MAXLINE], char buf[CONTENT_SIZE], size_t length);

/**
 * @brief   Destroy web cache.
 * 
*/
void cache_destory(void);