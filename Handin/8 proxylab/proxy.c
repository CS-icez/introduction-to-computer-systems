/**
 * @file    proxy.c
 * @author  icez_BaiTianYu 2100013011@stu.pku.edu.cn
 * @brief   proxylab main code
 * @date    2022.12.21 - 2022.12.22
 */

#include "csapp.h"
#include "cache.h"

#define POINTER_SUCCESS ((void *)0)
#define POINTER_ERROR   ((void *)-1)

typedef char string[MAXLINE];
typedef struct _URL{
    string hostname;
    string port;
    string uri;
} URL;

static void *business(void *_clientfd);
static void do_get(rio_t *client_rp, string url);
static void do_connect(rio_t *client_rp, string url);
static int parse_url(string url, URL *result);
static int read_header(rio_t *client_rp, string result, const string hostname);
static void *io_transfer(void *ptr);

int main(int argc, char *argv[]){
    int listenfd, clientfd;
    string hostname, port;
    socklen_t clientlen;
    struct sockaddr_storage clientaddr;
    pthread_t tid;

    if (argc != 2){
        fprintf(stderr, "usage: %s <port>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    listenfd = Open_listenfd(argv[1]);
    clientlen = sizeof(clientaddr);
    Signal(SIGPIPE, SIG_IGN);

    if (cache_init() == -1){
        Close(listenfd);
        exit(EXIT_FAILURE);
    }

    while (1){
        clientfd = accept(listenfd, (SA *)&clientaddr, &clientlen);
        if (clientfd < 0){
            fprintf(stderr, "accept error: %s\n", strerror(errno));
            continue;
        }
        if (getnameinfo((SA *)&clientaddr, clientlen, hostname, MAXLINE, port, MAXLINE, 0) < 0){
            fprintf(stderr, "getnameinfo error: %s\n", strerror(errno));
            continue;
        }
        Pthread_create(&tid, NULL, business, (void *)(long)clientfd);
    }

    cache_destory();
    Close(listenfd);
}

/**
 * @brief   Thread starter function. Detach the thread and do HTTP business.
 * @param   _cliendfd '(int)(long)_cliendfd' is client fd.
 * @return  'POINTER_SUCCESS' on success, 'POINTER_ERROR' on error.
 * 
*/
static void *business(void* _clientfd){
    if (pthread_detach(pthread_self()) < 0){
        fprintf(stderr, "pthread_detach error: %s\n", strerror(errno));
        return POINTER_ERROR;
    }

    int clientfd = (int)(long)_clientfd;
    string buf, method, url, version;
    rio_t client_rio;

    rio_readinitb(&client_rio, clientfd);

    if (rio_readlineb(&client_rio, buf, MAXLINE) <= 0){
        fprintf(stderr, "fail to read request line\n");
        return POINTER_ERROR;
    }
        
    if (sscanf(buf, "%s%s%s", method, url, version) != 3){
        fprintf(stderr, "malformed request line\n");
        return POINTER_ERROR;
    }

    if (!strcasecmp(method, "GET"))
        do_get(&client_rio, url);
    else if (!strcasecmp(method, "CONNECT"))
        do_connect(&client_rio, url);
    else
        fprintf(stderr, "unknown request method\n");

    Close(clientfd);

    return POINTER_SUCCESS;
}

/**
 * @brief   Deal with GET request.
 * @param   client_rp   pointer to the rio struct associated with client fd.
 * @param   url         request URL.
*/
static void do_get(rio_t *client_rp, string url){
    string buf, header;
    URL url_parsed;
    rio_t server_rio;
    int serverfd;
    ssize_t n;
    char response[CONTENT_SIZE];
    int response_p = 0;

    if ((n = cache_request(url, response)) != -1){  // cache hit
        if (rio_writen(client_rp->rio_fd, response, n) != n){
            fprintf(stderr, "rio_writen error\n");
        }
        return;
    }

    if (parse_url(url, &url_parsed)  < 0){
        fprintf(stderr, "malformed URL\n");
        return;
    }

    if (read_header(client_rp, header, url_parsed.hostname) < 0){
        fprintf(stderr, "malformed request header\n");
        return;
    }

    serverfd = open_clientfd(url_parsed.hostname, url_parsed.port);
    if (serverfd < 0){
        fprintf(stderr, "open_clientfd error: %s\n", strerror(errno));
        return;
    }
    rio_readinitb(&server_rio, serverfd);

    sprintf(buf, "GET %s HTTP/1.0\r\n%s", url_parsed.uri, header);  // cause warning, indeed unsafe, but who cares

    if (rio_writen(serverfd, buf, strlen(buf)) != strlen(buf)){
        fprintf(stderr, "rio_writen error\n");
        return;
    }

    while ((n = rio_readnb(&server_rio, buf, MAXLINE))){
        if (n == -1){
            fprintf(stderr, "rio_readlineb error\n");
            return;
        }

        if (response_p+n > CONTENT_SIZE)    // object too large
            response_p = -1;

        if (response_p != -1){
            memcpy(response+response_p, buf, n);
            response_p += n;
        }
        
        if (rio_writen(client_rp->rio_fd, buf, n) != n){
            fprintf(stderr, "rio_writen error\n");
            return;
        }
    }

    if (response_p != -1)
        cache_record(url, response, response_p);

    Close(serverfd);
}

/**
 * @brief   Parse URL. Part it into three fields.
 * @param   url     URL. Guaranteed unchanged after execution. Must be in the form http://...(: ...)/...
 * @param   result  Destination.
 * @return  0 on success, -1 on error
*/
static int parse_url(string url, URL *result){
    char *port, *uri;
    const int len = strlen("http://");

    if (strncmp(url, "http://", len))
        return -1;

    port = strchr(url+len, ':');
    uri  = strchr(url+len, '/');

    if (!uri) return -1;
    *uri = '\0';
    if (port) *port = '\0';
    else port = "\180";     // default port is 80

    strcpy(result->hostname, url+len);
    strcpy(result->port, port+1);
    *(result->uri) = '/';
    strcpy(result->uri+1, uri+1);

    *uri = '/';
    if (*port == '\0') *port = ':'; // string constant shouldn't be changed

    return 0;
}

/**
 * @brief   Read request header from client and store it in 'result'.
 * @param   client_rp   pointer to the rio struct associated with client fd.
 * @param   result      Destination.
 * @param   hostname    host name.
 * @return  0 on success, -1 on error.
*/
static int read_header(rio_t *client_rp, string result, const string hostname){
    string buf;
    string host = {};

    rio_readlineb(client_rp, buf, MAXLINE);
    while (strcmp(buf, "\r\n")){
        if (!strncmp(buf, "Host", strlen("Host")))
            strcpy(host, buf+strlen("Host: "));
        else if (strncmp(buf, "User-Agent", strlen("User-Agent")) && 
                 strncmp(buf, "Connection", strlen("Connection")) &&
                 strncmp(buf, "Proxy-Connection", strlen("Proxy-Connection"))){
            strcat(result, buf);
        }
        rio_readlineb(client_rp, buf, MAXLINE);
    }

    sprintf(result+strlen(result), "%s%s%s%s", "Host: ", *host? host: hostname, *host? "": "\r\n", 
            "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:10.0.3) Gecko/20120305 Firefox/10.0.3\r\n\
            Connection: close\r\nProxy-Connection: close\r\n\r\n");
    
    return 0;
}

/**
 * @brief   Deal with CONNECT request.
 * @param   client_rp   pointer to the rio struct associated with client fd.
 * @param   url         request URL.
*/
static void do_connect(rio_t *client_rp, string url){
    static string response = "HTTP/1.1 200 Connection Established\r\nConnection: close\r\n\r\n";
    string buf;
    URL url_parsed;
    rio_t server_rio;
    int serverfd;
    ssize_t n;
    char *port;
    pthread_t tid_c2s, tid_s2c;
    int *ptr_c2s, *ptr_s2c;

    port = strchr(url, ':');
    *port = '\0';
    strcpy(url_parsed.hostname, url);
    strcpy(url_parsed.port, port+1);
    *port = ':';
    
    serverfd = open_clientfd(url_parsed.hostname, url_parsed.port);
    if (serverfd < 0){
        fprintf(stderr, "open_clientfd error: %s\n", strerror(errno));
        return;
    }
    rio_readinitb(&server_rio, serverfd);

    n = rio_readlineb(client_rp, buf, MAXLINE);
    while (strcmp(buf, "\r\n")){
        if (n == -1){
            fprintf(stderr, "rio_readlineb error\n");
            return;
        }

        n = rio_readlineb(client_rp, buf, MAXLINE);
    }

    if (rio_writen(client_rp->rio_fd, response, strlen(response)) != strlen(response)){
        fprintf(stderr, "rio_writen error\n");
        return;
    }

    ptr_c2s = Malloc(2*sizeof(int));
    ptr_s2c = Malloc(2*sizeof(int));
    ptr_c2s[0] = ptr_s2c[1] = client_rp->rio_fd;
    ptr_c2s[1] = ptr_s2c[0] = serverfd;
    
    Pthread_create(&tid_c2s, NULL, io_transfer, ptr_c2s);
    Pthread_create(&tid_s2c, NULL, io_transfer, ptr_s2c);
    Pthread_join(tid_c2s, NULL);
    Pthread_join(tid_s2c, NULL);

    Close(serverfd);
}

/**
 * @brief   A simple I/O transfer station.
 * @param   ptr '((int*)ptr)[0]' is input end fd.
 *              '((int*)ptr)[1]' is output end fd.
 * @return  'POINTER_SUCCESS' on success, 'POINTER_ERROR' on error.
 * @note    'ptr' must dynamically allocated, as it will be 'free'd in this function.
*/
static void *io_transfer(void *ptr){
    string buf;
    int n;
    int readfd  = ((int*)ptr)[0];
    int writefd = ((int*)ptr)[1];
    free(ptr);

    while ((n = read(readfd, buf, MAXLINE))){
        if (n == -1 && errno != EINTR){
            fprintf(stderr, "read error: %s\n", strerror(errno));
            return POINTER_ERROR;
        } 

        if (rio_writen(writefd, buf, n) != n){
            fprintf(stderr, "rio_writen error\n");
            return POINTER_ERROR;
        }
    }

    return 0;
}