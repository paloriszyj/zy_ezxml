/**
 * \file zy_os_define.h
 *
 * \brief 
 *
 * \details 
 *
 * \copyright Copyright (c) 2018, zhiyun-tech \n
 *  All rights reserved.
 *
 * \author zhangwei
 * \date 2018-12-27
 * \version 0.0.1
 */

#ifndef _ZY_OS_DEFINE_H_
#define _ZY_OS_DEFINE_H_

#include <semaphore.h> //信号量
#include <sys/time.h>
#include "stdbool.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Common unsigned types */
#define ZY_DEFINED_U8
typedef unsigned char  U8;


#ifndef ZY_DEFINED_U16
#define ZY_DEFINED_U16
typedef unsigned short U16;
#endif

#ifndef ZY_DEFINED_U32
#define ZY_DEFINED_U32
typedef unsigned int   U32;
#endif

#ifndef ZY_DEFINED_U64
#define ZY_DEFINED_U64
typedef unsigned long long U64;
#endif

/** Common signed types */
#ifndef ZY_DEFINED_S8
#define ZY_DEFINED_S8
typedef signed char  S8;
#endif

#ifndef ZY_DEFINED_S16
#define ZY_DEFINED_S16
typedef signed short S16;
#endif

#ifndef ZY_DEFINED_S32
#define ZY_DEFINED_S32
typedef signed int S32;
#endif

#ifndef ZY_DEFINED_S64
#define ZY_DEFINED_S64
typedef signed long S64;
#endif

/** Common NULL */
#ifndef		NULL
#define		NULL	0
#endif

/** Common true false */
#ifndef ZY_TRUE 
#define ZY_TRUE		1
#endif

#ifndef ZY_FALSE
#define ZY_FALSE 	0
#endif


typedef const char* zy_const_char_t;
typedef void* zy_void_t;


#define ZY_INVALID_ID			((U32)(-1))
#define ZY_INVALID_HANDLE		((U32)(-1))

/** error code */
#define ZY_FAILED 									-1
#define ZY_SUCCESS 									0
#define ZY_ERROR_BAD_PARAMETER						1
#define ZY_ERROR_NO_MEMORY							2
#define ZY_ERROR_UNKNOWN_DEVICE 					3
#define ZY_ERROR_ALREADY_INITIALIZED 				5
#define ZY_ERROR_NO_FREE_HANDLES 					6
#define ZY_ERROR_INVALID_HANDLE						7
#define ZY_ERROR_INVALID_ID 						8
#define ZY_ERROR_FEATURE_NOT_SUPPORTED 				9
#define ZY_ERROR_INTERRUPT_INSTALL 					10
#define ZY_ERROR_INTERRUPT_UNINSTALL 				11
#define ZY_ERROR_TIMEOUT 							12
#define ZY_ERROR_DEVICE_BUSY 						13
#define ZY_ERROR_NO_INIT							14
#define ZY_ERROR_MAX_COUNT 							15
#define ZY_ERROR_STATUS_ENABLE 						16
#define ZY_ERROR_STATUS_DISABLE 					17
#define ZY_ERROR_LOADER_SUCCESS 					18
#define ZY_ERROR_IGNORE 							19

typedef void * (*zy_malloc_pfn_t) (U32 size);
typedef void	(* zy_free_pfn_t) (void * p_mem);

#define CHECK_INPUT_PARAMETER_VALUE_INT(value,parameter) \
	do \
	{ \
		if(value > parameter) \
		{ \
			printf("Invalid parameter!\n"); \
			return -1;\
		}\
	}\
	while(0)

#define CHECK_INPUT_PARAMETER_VALUE_UINT(value,parameter) \
	do \
	{ \
		if(value > parameter) \
		{ \
			printf("Invalid parameter!\n"); \
			return 0;\
		}\
	}\
	while(0)

#define CHECK_INPUT_PARAMETER_VALUE_NULL(value,parameter) \
	do \
	{ \
		if(value == parameter) \
		{ \
			printf("Invalid parameter!\n"); \
			return NULL;\
		}\
	}\
	while(0)


#define RECEIVE_SIZE 2*1024
#define ZY_DBG_PRINT_IMP  printf
#define ZY_MALLOC malloc
#define ZY_FREE(m) do {\
    if ( m != NULL ) \
        free(m); \
    }while(0)
#define ZY_GET_TIME(x) \
    do \
    { \
        struct timeval tv; \
        gettimeofday(&tv, NULL); \
        x = (U64)tv.tv_sec*1000000 + tv.tv_usec; \
    } while(0)


int os_sem_timeout(sem_t *os_sem,U32 time_ms);

#ifdef __cplusplus
}
#endif

#endif

