/**
 * \file zy_os_debug.h
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

#ifndef  _ZY_OS_DEBUG_H_
#define  _ZY_OS_DEBUG_H_
 
#include <stdlib.h>
#include <stdio.h>
 
#include "zy_os_define.h"

#define ZYDEBUG 1


#ifdef __cplusplus
 extern "C" {
#endif
 
 typedef enum zy_dbg_level_e
 {
 	 ZY_DBG_TEST = 0,
	 ZY_DBG_DEBUG ,
	 ZY_DBG_INFO,
	 ZY_DBG_ERROR,
	 ZY_DBG_NONE,
	 ZY_DBG_LEVEL_MAX
 }
 zy_dbg_level;
 
 typedef struct zy_dbg_module_info_s
 {
	 zy_dbg_level	 m_level; /*!< current level for this module */
	 const char 		 *m_name; /*!< name of the module */
	 U32				 m_inited;
 }
 zy_dbg_module_info; 



#define ZY_DBG_PRINT_STAGE2( level, module_info, msg ) \
	 do{ \
		 if ( level == ZY_DBG_ERROR ) \
		 { \
			 ZY_DBG_PRINT_IMP("\033[1;40;31m ");\
		 } \
		 else if ( level == ZY_DBG_DEBUG ) \
		 { \
			 ZY_DBG_PRINT_IMP("\033[1;40;32m ");\
		 } \
		 else if ( level == ZY_DBG_INFO ) \
		 { \
			 ZY_DBG_PRINT_IMP("\033[1;40;37m ");\
		 } \
		 ZY_DBG_PRINT_IMP msg; \
		 ZY_DBG_PRINT_IMP(" [%s %d] ",__FUNCTION__,__LINE__);\
		 if ( level == ZY_DBG_ERROR ) \
		 { \
			 ZY_DBG_PRINT_IMP("\033[0m ");\
		 } \
		 else if ( level == ZY_DBG_DEBUG ) \
		 { \
			 ZY_DBG_PRINT_IMP("\033[0m ");\
		 } \
		 else if ( level == ZY_DBG_INFO ) \
		 { \
			 ZY_DBG_PRINT_IMP("\033[0m ");\
		 } \
		 ZY_DBG_PRINT_IMP("\n");\
	 }while(0)
 
#define ZY_DBG_PRINT_STAGE1(level, msg) \
	 do { \
		 if ( (level != ZY_DBG_NONE) && (level >= s_this_module.m_level) ) \
		 { \
			  ZY_DBG_PRINT_STAGE2(level, s_this_module, msg); \
		 } \
	 } while (0)

#define ZY_DBG_PRINT(level, msg) \
		  do { \
			  if ( (level != ZY_DBG_NONE) && (level >= s_this_module.m_level) ) \
			  { \
				   ZY_DBG_PRINT_IMP msg; \
			  } \
		  } while (0)

#define ZY_DBG_MODULE_INFO_INITIALIZER(module_name) { ZY_DBG_ERROR, #module_name, ZY_FALSE }
 
#ifdef __GNUC__
#define ZY_MODULE(module_name) static zy_dbg_module_info __attribute__ ((__unused__)) s_this_module = ZY_DBG_MODULE_INFO_INITIALIZER(module_name)
#else
#define ZY_MODULE(module_name) static zy_dbg_module_info s_this_module = ZY_DBG_MODULE_INFO_INITIALIZER(module_name)
#endif
 
 
#define ZY_INFO( msg ) ZY_DBG_PRINT_STAGE1( ZY_DBG_INFO, msg )
#define ZY_ERROR( msg ) ZY_DBG_PRINT_STAGE1( ZY_DBG_ERROR, msg )

#ifdef ZYDEBUG
#define ZY_PRINTF( msg ) ZY_DBG_PRINT(ZY_DBG_TEST, msg)
#define ZY_DEBUG( msg ) ZY_DBG_PRINT_STAGE1( ZY_DBG_DEBUG, msg )
#define ZY_TEST( msg ) ZY_DBG_PRINT_STAGE1( ZY_DBG_TEST, msg )
#else
#define ZY_TEST( msg ) ((void)0)
#define ZY_PRINTF( msg ) ((void)0)
#define ZY_DEBUG( msg ) ((void)0)
#endif  /* ZYDEBUG */
 
#define ZY_MODULE_SET_DBG_LEVEL( level ) \
	 do { \
		 if ( (level >= 0) && (level < ZY_DBG_LEVEL_MAX) ) \
			 s_this_module.m_level = level; \
	 } while(0)
 
#define ZY_MODULE_GET_DBG_LEVEL() \
	 do { \
			 return s_this_module.m_level; \
	 } while(0)
 
#define ZY_MODULE_SET_INIT_STATUS( val ) \
	 do { \
			 s_this_module.m_inited = val; \
	 } while(0)
 
#if 1
#define ZY_MODULE_CHECK_INIT()\
	 do{\
		 if (! s_this_module.m_inited )\
		 {\
			 ZY_ERROR(("This module MUST be initialized first!"));\
			 return ZY_ERROR_NO_INIT;\
		 }\
 }while (0)
#else
#define ZY_MODULE_CHECK_INIT() ((void)0)
#endif
 
#if 1
#define ZY_MODULE_CHECK_PRAM_PTRS( ptrs )\
	 do{\
		 if ( ! (ptrs) )\
		 {\
			 ZY_ERROR(( #ptrs "should NOT be NULL!"));\
			 return ZY_ERROR_BAD_PARAMETER;\
		 }\
 }while (0)

#define ZY_MODULE_CHECK_RET( value ) \
    do { \
        if(ZY_SUCCESS != value) \
        { \
            ZY_ERROR(("Return value: %d", value)); \
            return value; \
        } \
    }while(0)

#define ZY_MODULE_CHECK_RET_TYPE( value , type ) \
    do { \
        if(ZY_SUCCESS != value) \
        { \
            ZY_ERROR(("Return value: %d", value)); \
            return (type)value; \
        } \
    }while(0)

#else
#define ZY_MODULE_CHECK_PRAM_PTRS( ptrs ) ((void)0)
#define ZY_MODULE_CHECK_RET( value ) ((void)0)
#endif
 
 //zy_revision_t	zy_dbg_get_revision(void);


 
#ifdef __cplusplus
 }
#endif
 
#endif   /* ----- #ifndef SK_HDI_DBG_INC  ----- */



