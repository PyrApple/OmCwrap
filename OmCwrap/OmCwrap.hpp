/************************************************************************************/
/*!
 *  @file       OmCwrap.h
 *  @brief      Examples of C wrapper for OM Lisp
 *  @author     David Poirier-Quinot
 *  @date       01/09/2016
 *
 */
/************************************************************************************/
#ifndef _OM_CWRAP_H__
#define _OM_CWRAP_H__

/// @n for now this is only for mac (gcc or clang)
#define OM_CWRAP_VISIBILITY_DEFAULT	__attribute__ ((visibility ("default")))

// use this export macro to expose public method to the dylib
#ifdef __cplusplus
#define OM_CWRAP_C_EXPORTS   extern "C"
#else
#define OM_CWRAP_C_EXPORTS
#endif

#define OM_CWRAP_API OM_CWRAP_C_EXPORTS OM_CWRAP_VISIBILITY_DEFAULT

#include <stdbool.h>    ///< include boolean for C interface
#include <iostream>

/************************************************************************************/

OM_CWRAP_API
const char * PrintChar() { return "hello world"; }

OM_CWRAP_API
int WrapInt (int inValue) { return inValue + 1; }

OM_CWRAP_API
unsigned int WrapUnsignedInt (unsigned int inValue) { return inValue * 10; }

OM_CWRAP_API
float WrapFloat (float inValue) { return inValue + 0.1f; }

/************************************************************************************/

struct OM_CWRAP_VISIBILITY_DEFAULT OmList
{
    unsigned int size;
    float * data;
};

OM_CWRAP_API
void WrapList (const OmList * inValue)
{
    std::cout << "list size: " << inValue->size << "\n";
    std::cout << "list pointer: " << inValue->data << "\n";
    
    std::cout << "list values: ";
    for( int i = 0; i < inValue->size; i++ )
    {
        std::cout << " " << inValue->data[i];
        inValue->data[i] *= 2;
    }
    std::cout << "\n \n";
}

/************************************************************************************/

struct OM_CWRAP_VISIBILITY_DEFAULT OmAudioBuffer
{
    unsigned int numChannels;  // < number of channels
    unsigned int numSamples;  // < number of samples (for each channel)
    float ** data;             // < data[ channelIndex ][ sampleIndex ]
};

typedef struct OmAudioBuffer OmAudioBuffer;     ///< C-style declaration


OM_CWRAP_API
void WrapSound(OmAudioBuffer * const bufferIn)
{
    std::cout << "\nnum channels: " << bufferIn->numChannels << "\n";
    std::cout << "num samples per channel: " << bufferIn->numSamples << "\n";
    std::cout << "audio data (pointer): " << bufferIn->data << "\n";
    
    // std::cout << "audio data (values):";
    for( int i = 0; i < bufferIn->numChannels; i++ )
    {
        // std::cout << "\nchannel " << i << ": \n";
        for( int j = 0; j < bufferIn->numSamples; j++ )
        {
            // std::cout << " " << bufferIn->data[i][j];
            bufferIn->data[i][j] *= 0.5f;
        }
    }
    std::cout << "\n";
}


#endif /* _OM_CWRAP_H__ */
