/* ezxml.h
 *
 * Copyright 2004-2006 Aaron Voisine <aaron@voisine.org>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _EZXML_H
#define _EZXML_H

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <fcntl.h>

#ifdef __cplusplus
extern "C" {
#endif

#define FBC_API_PUBLIC __attribute__((visibility ("default")))
#define FBC_API_LOCAL __attribute__((visibility("hidden")))

#define EZXML_BUFSIZE 1024 // size of internal memory buffers
#define EZXML_NAMEM   0x80 // name is malloced
#define EZXML_TXTM    0x40 // txt is malloced
#define EZXML_DUP     0x20 // attribute name and value are strduped
#define MAX_ATTRIBUTES_SIZE    64
#define DISPLAYMAX    10
#define STRINGSNUM    6

#define MAXINFO     1000
#define XMLMAXINFO  1500

#define STEP_C 0
#define STEP_D 2
#define STEP_E 4
#define STEP_F 5
#define STEP_G 7
#define STEP_A 9
#define STEP_B 11

#define Z_OCTAVE 1

typedef struct zy_com_playinfo_s {  
    int measure;	//小节序号 0 based
    int chords;		//小节序号内的和弦序号(新小节，该序号清零)
    int note;		//和弦时此标记不使用
}zy_com_playinfo_t;

typedef struct ezxml *ezxml_t;
struct ezxml {
    char *name;      // tag name
    char **attr;     // tag attributes { name, value, name, value, ... NULL }
    char *txt;       // tag character content, empty string if none
    size_t off;      // tag offset from start of parent tag character content
    ezxml_t next;    // next tag with same name in this section at this depth
    ezxml_t sibling; // next tag with different name in same section and depth
    ezxml_t ordered; // next tag, same section and depth, in original order
    ezxml_t child;   // head of sub tag list, NULL if none
    ezxml_t parent;  // parent tag, NULL if current tag is root tag
    short flags;     // additional information
};

typedef struct zy_note_info_s
{
    int ending_number;
    char step[MAX_ATTRIBUTES_SIZE];    //音阶
    char octave[MAX_ATTRIBUTES_SIZE];  //八度
    char alter[MAX_ATTRIBUTES_SIZE];   //表示升降音，-1 表示降音，1 表示升音
    char tie[MAX_ATTRIBUTES_SIZE];     //表示一个连音的开始和结束
    int  rest;                         //休止符
    int chordsign;                     //和弦，叠音，也就是说当这个标签出现的时候，竖直方向是有多个音的  
    int measure;	//小节序号 0 based
    int chords;		//小节序号内的和弦序号(新小节，该序号清零)
    int note;		//和弦序号内的音符序号(新和弦，该序号清零) 
    char string[MAX_ATTRIBUTES_SIZE];   //记录note所在的弦
} zy_note_info_t;

enum repeateType
{
    FORWARDTOBACKWARD = 0x00,
    SEGNOTODALSEGNO,
};
typedef struct zy_repeate_info_s
{
    int repeate_type;  //[forward,backward]类型为1,  [segno,dalsegno]类型为2
    int start;       //如果是前奏，则表示开始反复的小结编号，如果是和弦，表示的是地几个和弦序号
    int end;         //如果是前奏，则表示开始反复的小结编号，如果是和弦，表示的是地几个和弦序号
} zy_repeate_info_t;

typedef struct zy_score_s
{
    int m_note_total;
    int m_note_cur;
    int m_note_if_parsing;
    int m_repeate_total;          //表示所有反复数量
    int m_repeate_cur;
    int m_solo_if_parsing_end;    //前奏是否解析结束
    int m_ending_number;
    zy_repeate_info_t m_repeate_info[MAX_ATTRIBUTES_SIZE];
    zy_note_info_t m_note_info[];
} zy_score_t;

typedef struct zy_harmony_frame_note_info_s
{
    char string[MAX_ATTRIBUTES_SIZE];
    char fret[MAX_ATTRIBUTES_SIZE];
    char barre[MAX_ATTRIBUTES_SIZE];
} zy_harmony_frame_note_info_t;

typedef struct zy_harmony_info_s
{    
    int ending_number;
    int m_framenote_total;     
    char root_step[MAX_ATTRIBUTES_SIZE];
    char root_alter[MAX_ATTRIBUTES_SIZE];
    char kind[MAX_ATTRIBUTES_SIZE];   
    zy_harmony_frame_note_info_t framenote[STRINGSNUM];    //表示构成和弦内的所有音高，记录的是品格图的内容
    zy_com_playinfo_t zy_display_harmony; 
} zy_harmony_info_t;

typedef struct zy_harmony_s
{
    int m_harmony_total;
    int m_harmony_cur;
    int m_harmony_if_parsing;
    int m_is_parsing_note;        //用来区分是属于前奏还是和弦的note
    int m_repeate_total;          //表示所有反复数量
    int m_repeate_cur; 
    int m_sound_cur; 
    int m_ending_number;
    zy_repeate_info_t m_repeate_info[MAX_ATTRIBUTES_SIZE];    //保存所有反复的小节段落
    zy_harmony_info_t m_harmony_info[];
} zy_harmony_t;

typedef struct zy_solo_display_s
{
    int zy_display_total;    //显示信息，每个节拍上共有多少个和弦，即竖线信息，显示连音线连接的所以note
    zy_com_playinfo_t zy_display_solo[DISPLAYMAX];         //吉他最多只有6根弦
} zy_solo_display_t;

typedef struct zy_solo_beat_s
{
    int zy_strings_total;    //每个节拍上共有多少个和弦，即竖线信息,连音线只算第一个
    int zy_beats[STRINGSNUM];         //吉他最多只有6根弦,记录note的音高
    int zy_strings[STRINGSNUM];       //吉他最多只有6根弦,记录note所在的弦
    zy_com_playinfo_t zy_score_display[STRINGSNUM];  //记录每个弦的位置
    zy_solo_display_t m_display_info;   //这个结构体解析了连音线上的所有音符，现在不用了，以防后面需要，没有删除
} zy_solo_beat_t;

typedef struct zy_solo_num_s
{
    int startmeasure;              //记录前奏间奏尾奏开始的小节
    int endmeasure;                //记录前奏间奏尾奏结束的小节
    int solonum;              //记录前奏间奏尾奏note数量
} zy_solo_num_t;

typedef struct zy_solo_s
{
    int play_measure;               //记录正在正在弹奏的小节
    int zy_beats_total;             //前奏所有note节点数量，连音线只算一个
    int solo_num_total;
    zy_solo_num_t solo_num_info[MAX_ATTRIBUTES_SIZE];    //记录前奏间奏尾奏信息
    zy_solo_beat_t m_beat_info[];   //前奏每个节拍信息
} zy_solo_t;

typedef struct zy_chord_strings_s
{
    char chordname[MAX_ATTRIBUTES_SIZE];
    int zy_strings_total;
    int zy_strings[STRINGSNUM];
    zy_com_playinfo_t zy_chord_display;
} zy_chord_strings_t;

typedef struct zy_chord_s
{
    int zy_chord_total;
    zy_chord_strings_t m_chord_info[];
} zy_chord_t;
//============================================================//
//第二步：对提取出来的xml信息进行处理，转换成需要的音符信息
enum playType
{
    NONE=0x00,
    SOLO,
    CHORDS,
};
enum barreType
{
    BARREEND=0x00,
    BARRESTART,
};

enum endingType
{
    ENDINGNONE=0x00,
    ENDINGSTART,
    DISCONTINUE,
    ENDINGSTOP,
};

enum playTime
{
    S_NONE=0x00,
    S_ONE,
    S_TWO,
};

enum repeatType
{
    REPEATNONE=0x00,
    FORWARD,
    BACKWARD,
};

enum guitarType
{
    ZY_FALSE=0x00,
    ZY_TURE,
};

typedef struct zy_guitar_string_s
{
    int zy_beats;         //吉他最多只有6根弦,记录note的音高
    int zy_strings;       //吉他最多只有6根弦,记录note所在的弦
} zy_guitar_string_t;

typedef struct zy_guitar_solo_s
{
    // int times;
    int forward;
    zy_guitar_string_t m_string_info[STRINGSNUM];
} zy_guitar_solo_t;

typedef struct zy_guitar_chord_s
{

} zy_guitar_chord_t;

typedef struct zy_guitar_s
{
    int m_type;          //前奏or和弦
    zy_guitar_solo_t m_solo_info;
    zy_guitar_chord_t m_chords_info;
    zy_com_playinfo_t m_display_info;
} zy_guitar_t;

typedef struct zy_info_s
{    
    int m_guitar_cur;
    int m_repeat_total;          //表示所有反复数量
    zy_guitar_t m_guitar_info[MAXINFO];
    zy_repeate_info_t m_repeate_info[MAX_ATTRIBUTES_SIZE];    //保存所有反复的小节段落
} zy_info_t;

//=====================================================//
//第一步:解析乐谱信息，不进行处理，单纯提取xml信息
typedef struct zy_xml_solo_s
{
    char step[MAX_ATTRIBUTES_SIZE];    //音阶
    char octave[MAX_ATTRIBUTES_SIZE];  //八度
    char alter[MAX_ATTRIBUTES_SIZE];   //表示升降音，-1 表示降音，1 表示升音
    char tie[MAX_ATTRIBUTES_SIZE];     //表示一个连音的开始和结束
    int  rest;                         //休止符
    int chordsign;                     //和弦，叠音，也就是说当这个标签出现的时候，竖直方向是有多个音的   
    char string[MAX_ATTRIBUTES_SIZE];   //记录note所在的弦
}zy_xml_solo_t;

typedef struct zy_harmony_barre_start_s
{
    int m_framenote_cur;
    int start_falg;
    char string[MAX_ATTRIBUTES_SIZE];
    char fret[MAX_ATTRIBUTES_SIZE];
} zy_harmony_barre_start_t;

typedef struct zy_xml_chord_s
{    
    char root_step[MAX_ATTRIBUTES_SIZE];
    char root_alter[MAX_ATTRIBUTES_SIZE];
    char kind[MAX_ATTRIBUTES_SIZE];   
    zy_harmony_frame_note_info_t framenote[STRINGSNUM];    //表示构成和弦内的所有音高，记录的是品格图的内容
    zy_harmony_barre_start_t barre_start_info;     //记录有横按开始的信息
}zy_xml_chord_t;

typedef struct zy_xml_info_s
{    
    int m_type;         //前奏or和弦
    int repeats;        //每个前奏or和弦的播放次数，
    int ending_number;  //记录反复跳跃记号上标数字
    zy_xml_solo_t m_solo_xml_info;
    zy_xml_chord_t m_chords_xml_info;
    zy_com_playinfo_t m_display_xml_info;
}zy_xml_info_t;

typedef struct zy_ending_info_s
{    
    int endingflag;     //反复开始时2置为REPEATSTART
    int ending_number;  //记录反复跳跃记号上标数字
}zy_ending_info_t;

typedef struct zy_xml_s
{
    int m_type;          //前奏or和弦
    int cur_xml;
    int endingflag;    //反复跳跃记号开始，继续，结束的判断标志位
    int repeatflag;    //反复开始，结束的判断标志位
    int firstbackward;  //第一个反复结束的标志位，因为第一组反复可能存在直接结束标签，没有开始标签，需要特殊处理
    zy_ending_info_t m_ending_info;
    zy_xml_info_t m_xml_info[XMLMAXINFO];
} zy_xml_t;

// Given a string of xml data and its length, parses it and creates an ezxml
// structure. For efficiency, modifies the data by adding null terminators
// and decoding ampersand sequences. If you don't want this, copy the data and
// pass in the copy. Returns NULL on failure.
FBC_API_LOCAL ezxml_t ezxml_parse_str(char *s, size_t len);

// A wrapper for ezxml_parse_str() that accepts a file descriptor. First
// attempts to mem map the file. Failing that, reads the file into memory.
// Returns NULL on failure.
FBC_API_LOCAL ezxml_t ezxml_parse_fd(int fd);

// a wrapper for ezxml_parse_fd() that accepts a file name
FBC_API_LOCAL ezxml_t ezxml_parse_file(const char *file);
    
// Wrapper for ezxml_parse_str() that accepts a file stream. Reads the entire
// stream into memory and then parses it. For xml files, use ezxml_parse_file()
// or ezxml_parse_fd()
FBC_API_LOCAL ezxml_t ezxml_parse_fp(FILE *fp);

// returns the first child tag (one level deeper) with the given name or NULL
// if not found
FBC_API_LOCAL ezxml_t ezxml_child(ezxml_t xml, const char *name);

// returns the next tag of the same name in the same section and depth or NULL
// if not found
#define ezxml_next(xml) ((xml) ? xml->next : NULL)

// Returns the Nth tag with the same name in the same section at the same depth
// or NULL if not found. An index of 0 returns the tag given.
FBC_API_LOCAL ezxml_t ezxml_idx(ezxml_t xml, int idx);

// returns the name of the given tag
#define ezxml_name(xml) ((xml) ? xml->name : NULL)

// returns the given tag's character content or empty string if none
#define ezxml_txt(xml) ((xml) ? xml->txt : "")

// returns the value of the requested tag attribute, or NULL if not found
FBC_API_LOCAL const char *ezxml_attr(ezxml_t xml, const char *attr);

// Traverses the ezxml sturcture to retrieve a specific subtag. Takes a
// variable length list of tag names and indexes. The argument list must be
// terminated by either an index of -1 or an empty string tag name. Example: 
// title = ezxml_get(library, "shelf", 0, "book", 2, "title", -1);
// This retrieves the title of the 3rd book on the 1st shelf of library.
// Returns NULL if not found.
FBC_API_LOCAL ezxml_t ezxml_get(ezxml_t xml, ...);

// Converts an ezxml structure back to xml. Returns a string of xml data that
// must be freed.
FBC_API_LOCAL char *ezxml_toxml(ezxml_t xml);

// returns a NULL terminated array of processing instructions for the given
// target
FBC_API_LOCAL const char **ezxml_pi(ezxml_t xml, const char *target);

// frees the memory allocated for an ezxml structure
FBC_API_LOCAL void ezxml_free(ezxml_t xml);
    
// returns parser error message or empty string if none
FBC_API_LOCAL const char *ezxml_error(ezxml_t xml);

// returns a new empty ezxml structure with the given root tag name
FBC_API_LOCAL ezxml_t ezxml_new(const char *name);

// wrapper for ezxml_new() that strdup()s name
#define ezxml_new_d(name) ezxml_set_flag(ezxml_new(strdup(name)), EZXML_NAMEM)

// Adds a child tag. off is the offset of the child tag relative to the start
// of the parent tag's character content. Returns the child tag.
FBC_API_LOCAL ezxml_t ezxml_add_child(ezxml_t xml, const char *name, size_t off);

// wrapper for ezxml_add_child() that strdup()s name
#define ezxml_add_child_d(xml, name, off) \
    ezxml_set_flag(ezxml_add_child(xml, strdup(name), off), EZXML_NAMEM)

// sets the character content for the given tag and returns the tag
FBC_API_LOCAL ezxml_t ezxml_set_txt(ezxml_t xml, const char *txt);

// wrapper for ezxml_set_txt() that strdup()s txt
#define ezxml_set_txt_d(xml, txt) \
    ezxml_set_flag(ezxml_set_txt(xml, strdup(txt)), EZXML_TXTM)

// Sets the given tag attribute or adds a new attribute if not found. A value
// of NULL will remove the specified attribute. Returns the tag given.
FBC_API_LOCAL ezxml_t ezxml_set_attr(ezxml_t xml, const char *name, const char *value);

// Wrapper for ezxml_set_attr() that strdup()s name/value. Value cannot be NULL
#define ezxml_set_attr_d(xml, name, value) \
    ezxml_set_attr(ezxml_set_flag(xml, EZXML_DUP), strdup(name), strdup(value))

// sets a flag for the given tag and returns the tag
FBC_API_LOCAL ezxml_t ezxml_set_flag(ezxml_t xml, short flag);

// removes a tag along with its subtags without freeing its memory
FBC_API_LOCAL ezxml_t ezxml_cut(ezxml_t xml);

// inserts an existing tag into an ezxml structure
FBC_API_LOCAL ezxml_t ezxml_insert(ezxml_t xml, ezxml_t dest, size_t off);

// Moves an existing tag to become a subtag of dest at the given offset from
// the start of dest's character content. Returns the moved tag.
#define ezxml_move(xml, dest, off) ezxml_insert(ezxml_cut(xml), dest, off)

// removes a tag along with all its subtags
#define ezxml_remove(xml) ezxml_free(ezxml_cut(xml))

FBC_API_LOCAL void xml_parse_init(void);

FBC_API_LOCAL void xml_parse_node(ezxml_t xml);

FBC_API_LOCAL void xml_solo_chord_free(void);

FBC_API_LOCAL void guitar_xml_parse_note(ezxml_t xml);

FBC_API_LOCAL void guitar_parse_xml_dump(void);

extern zy_solo_t *ptr_solo;

extern zy_chord_t *ptr_chord;

#ifdef __cplusplus
}
#endif

#endif // _EZXML_H