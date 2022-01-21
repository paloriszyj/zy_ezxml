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

#define STEP_C 0
#define STEP_D 2
#define STEP_E 4
#define STEP_F 5
#define STEP_G 7
#define STEP_A 9
#define STEP_B 11

#define Z_OCTAVE 1


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
    char step[MAX_ATTRIBUTES_SIZE];    //音阶
    char octave[MAX_ATTRIBUTES_SIZE];  //八度
    char alter[MAX_ATTRIBUTES_SIZE];   //表示升降音，-1 表示降音，1 表示升音
    char tie[MAX_ATTRIBUTES_SIZE];     //表示一个连音的开始和结束
    int  rest;                         //休止符
    int chordsign;                     //和弦，叠音，也就是说当这个标签出现的时候，竖直方向是有多个音的  
    int measure;	//小节序号 0 based
    int chords;		//小节序号内的和弦序号(新小节，该序号清零)
    int note;		//和弦序号内的音符序号(新和弦，该序号清零)            
} zy_note_info_t;

typedef struct zy_score_s
{
    int m_note_total;
    int m_note_cur;
    int m_note_if_parsing;
    zy_note_info_t m_note_info[];
} zy_score_t;

enum repeateType
{
    FORWARDTOBACKWARD = 0x00,
    SEGNOTODALSEGNO,
};
typedef struct zy_repeate_info_s
{
    int repeate_type;  //[forward,backward]类型为1,  [segno,dalsegno]类型为2
    int start;       //开始反复的小结编号
    int end;         //小结结束后，需要反复，记录当前小结   
} zy_repeate_info_t;

typedef struct zy_harmony_frame_note_info_s
{
    char string[MAX_ATTRIBUTES_SIZE];
    char fret[MAX_ATTRIBUTES_SIZE];
    char barre[MAX_ATTRIBUTES_SIZE];
} zy_harmony_frame_note_info_t;

typedef struct zy_harmony_notes_info_s
{
    char string[MAX_ATTRIBUTES_SIZE];
    int measure;	//小节序号 0 based
    int chords;		//小节序号内的和弦序号(新小节，该序号清零)
    int note;		//和弦序号内的音符序号(新和弦，该序号清零)
} zy_harmony_notes_info_t;

typedef struct zy_harmony_info_s
{    
    int words;   //为解析横按添加的文字说明
    int ending_number;
    int m_framenote_total;
    int harmony_notes_cur;
    int harmony_notes_total;
    int m_harmony_note_if_parsing;       //解析和弦内的note
    char root_step[MAX_ATTRIBUTES_SIZE];
    char root_alter[MAX_ATTRIBUTES_SIZE];
    char kind[MAX_ATTRIBUTES_SIZE];   
    zy_harmony_frame_note_info_t framenote[STRINGSNUM];    //表示构成和弦内的所有音高，记录的是品格图的内容
    zy_harmony_notes_info_t notes[MAX_ATTRIBUTES_SIZE];   //表示构成和弦内弹奏顺序指法
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

typedef struct zy_solo_display_site_s
{
    int measure;	//小节序号 0 based
    int chords;		//小节序号内的和弦序号(新小节，该序号清零)
    int note;		//和弦序号内的音符序号(新和弦，该序号清零)
} zy_solo_display_site_t;

typedef struct zy_solo_display_s
{
    int zy_display_total;    //显示信息，每个节拍上共有多少个和弦，即竖线信息，显示连音线连接的所以note
    zy_solo_display_site_t zy_display[DISPLAYMAX];         //吉他最多只有6根弦
} zy_solo_display_t;

typedef struct zy_solo_beat_s
{
    int zy_strings_total;    //每个节拍上共有多少个和弦，即竖线信息,连音线只算第一个
    int zy_beats[STRINGSNUM];         //吉他最多只有6根弦
    zy_solo_display_t m_display_info;
} zy_solo_beat_t;

typedef struct zy_solo_s
{
    int zy_beats_total;             //前奏所有note节点数量，连音线只算一个
    zy_solo_beat_t m_beat_info[];   //前奏每个节拍信息
} zy_solo_t;

typedef struct zy_chord_display_site_s
{
    int stringnum;  //和弦内没根系弹奏的次数
    int curnum;  //和弦内没根系弹奏的次数
    zy_solo_display_site_t zy_display[DISPLAYMAX];  
} zy_chord_display_site_t;

typedef struct zy_chord_strings_s
{
    char chordname[MAX_ATTRIBUTES_SIZE];
    int zy_strings_total;
    int zy_strings[STRINGSNUM];
    zy_chord_display_site_t zy_chord_display[STRINGSNUM]
} zy_chord_strings_t;

typedef struct zy_chord_s
{
    int zy_chord_total;
    zy_chord_strings_t m_chord_info[];
} zy_chord_t;

static struct zy_playinfo_s {  
    int measure;	//小节序号 0 based
    int chords;		//小节序号内的和弦序号(新小节，该序号清零)
    int note;		//和弦序号内的音符序号(新和弦，该序号清零)
};


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

extern zy_solo_t *ptr_solo;

extern zy_chord_t *ptr_chord;

#ifdef __cplusplus
}
#endif

#endif // _EZXML_H