#ifndef PLAYER_CLASS_H
#define PLAYER_CLASS_H

#include <string>
#include <octave/oct.h>
#include <octave/ov-int32.h>
#include <portaudio.h>

#include "player_class.h"

enum audio_type {INT8, UINT8, INT16, DOUBLE};

class audioplayer : public octave_base_value 
{
public:
    audioplayer();
    ~audioplayer();
    // Overloaded base functions
    double player_value() const { return 0; }
    virtual double scalar_value (bool frc_str_conv = false) const 
    {
        return 0;
    }
    void print (std::ostream& os, bool pr_as_read_syntax = false) const;
    void print_raw (std::ostream& os, bool pr_as_read_syntax) const;
    // Properties
    bool is_constant (void) const { return true;}
    bool is_defined (void) const { return true;}
    bool print_as_scalar (void) const { return true;}

    void init();
    void init_fn();
    void set_y(octave_value y);
    void set_y(octave_function *fn);
    void set_y(std::string fn);
    Matrix &get_y();
    RowVector *get_left();
    RowVector *get_right();
    void set_fs(int fs);
    int get_fs();
    void set_nbits(int nbits);
    int get_nbits();
    void set_id(int id);
    int get_id();
    int get_channels();
    audio_type get_type();

    void set_sample_number(unsigned int sample);
    unsigned int get_sample_number();
    unsigned int get_total_samples();
    void set_end_sample(unsigned int sample);
    unsigned int get_end_sample();
    void reset_end_sample();
    void set_tag(charMatrix tag);
    charMatrix get_tag();
    void set_userdata(octave_value userdata);
    octave_value get_userdata();
    PaStream *get_stream();
    octave_function *octave_callback_function;

    void playblocking();
    void play();
    void pause();
    void resume();
    void stop();
    bool isplaying();
private:
    Matrix y;
    RowVector left;
    RowVector right;
    charMatrix tag;
    octave_value userdata;
    int channels;
    int fs;
    int nbits;
    int id;
    unsigned int sample_number;
    unsigned int end_sample;
    PaStream *stream;
    PaStreamParameters output_parameters;
    audio_type type;
    DECLARE_OCTAVE_ALLOCATOR
    DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif // PLAYER_CLASS_H
