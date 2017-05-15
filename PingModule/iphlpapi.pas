//---------------------------------------------------------------------
//         IPHlpAPI.pas  part of Ping Demo
//-----------------------------------------------------------------
//  DISCLAIMER:
//      This software is provided 'as-is', without any express or
//      implied warranty.  In no event will the author be held liable
//      for any damages arising from the use of this software.
//-----------------------------------------------------------------
//  DESCRIPTION:
//      This is a basic representation of what is contained in
//      the Win32 API file ICMP.DLL.  This file is based on a
//      USENET post by Mike Cariotoglou (full information follows).
//
//      For my purposes, I extended Ticmp_echo_reply and called this
//      extension TsmICMP_Echo_Reply.  The reason for this is to allow
//      a simple way of providing a reply buffer.  While there are
//      certainly more robust ways of accomplishing this, this does get
//      the job done.
//---------------------------------------------------------------------
//  REFERENCES:
//      USENET Post By:  Mike Cariotoglou (mike@REMOVETHIS.singular.gr)
//             Subject:  "Re: PING and DELPHI"
//           Newsgroup:  borland.public.delphi.internet
//                Date:  1999/05/18  (May 18, 1999)
//---------------------------------------------------------------------



unit IPHlpAPI;

interface

uses windows;

Const


//
// IP_STATUS codes returned from IP APIs
//
   IP_STATUS_BASE                 = 11000;
   IP_SUCCESS                     = 0;
   IP_BUF_TOO_SMALL               = (IP_STATUS_BASE + 1);
   IP_DEST_NET_UNREACHABLE        = (IP_STATUS_BASE + 2);
   IP_DEST_HOST_UNREACHABLE       = (IP_STATUS_BASE + 3);
   IP_DEST_PROT_UNREACHABLE       = (IP_STATUS_BASE + 4);
   IP_DEST_PORT_UNREACHABLE       = (IP_STATUS_BASE + 5);
   IP_NO_RESOURCES                = (IP_STATUS_BASE + 6);
   IP_BAD_OPTION                  = (IP_STATUS_BASE + 7);
   IP_HW_ERROR                    = (IP_STATUS_BASE + 8);
   IP_PACKET_TOO_BIG              = (IP_STATUS_BASE + 9);
   IP_REQ_TIMED_OUT               = (IP_STATUS_BASE + 10);
   IP_BAD_REQ                     = (IP_STATUS_BASE + 11);
   IP_BAD_ROUTE                   = (IP_STATUS_BASE + 12);
   IP_TTL_EXPIRED_TRANSIT         = (IP_STATUS_BASE + 13);
   IP_TTL_EXPIRED_REASSEM         = (IP_STATUS_BASE + 14);
   IP_PARAM_PROBLEM               = (IP_STATUS_BASE + 15);
   IP_SOURCE_QUENCH               = (IP_STATUS_BASE + 16);
   IP_OPTION_TOO_BIG              = (IP_STATUS_BASE + 17);
   IP_BAD_DESTINATION             = (IP_STATUS_BASE + 18);
//
// The next group are status codes passed up on status indications to
// transport layer protocols.
//
   IP_ADDR_DELETED                = (IP_STATUS_BASE + 19);
   IP_SPEC_MTU_CHANGE             = (IP_STATUS_BASE + 20);
   IP_MTU_CHANGE                  = (IP_STATUS_BASE + 21);
   IP_UNLOAD                      = (IP_STATUS_BASE + 22);
   IP_GENERAL_FAILURE             = (IP_STATUS_BASE + 50);
   MAX_IP_STATUS                  = IP_GENERAL_FAILURE;
   IP_PENDING                     = (IP_STATUS_BASE + 255);

//
// Values used in the IP header Flags field.
//
   IP_FLAG_DF                     = $2;        //  Don't fragment this packet.

//
// Supported IP Option Types.
//
// These types define the options which may be used in the OptionsData field
// of the ip_option_information structure.  See RFC 791 for a complete
// description of each.
//
   IP_OPT_EOL                     = 0;         //  End of list option
   IP_OPT_NOP                     = 1;         //  No operation
   IP_OPT_SECURITY                = $82;       //  Security option
   IP_OPT_LSRR                    = $83;       //  Loose source route
   IP_OPT_SSRR                    = $89;       //  Strict source route
   IP_OPT_RR                      = $7;        //  Record route
   IP_OPT_TS                      = $44;       //  Timestamp
   IP_OPT_SID                     = $88;       //  Stream ID (obsolete)

   MAX_OPT_SIZE                   = 40;        //  Maximum length of IP options in bytes


Type

 TIPAddr=integer;     // An IP address.
 TIPMask=integer;     // An IP subnet mask.
 TIP_STATUS=Integer;  // Status code returned from IP APIs.

//
// The ip_option_information structure describes the options to be
// included in the header of an IP packet. The TTL, TOS, and Flags
// values are carried in specific fields in the header. The OptionsData
// bytes are carried in the options area following the standard IP header.
// With the exception of source route options, this data must be in the
// format to be transmitted on the wire as specified in RFC 791. A source
// route option should contain the full route - first hop thru final
// destination - in the route data. The first hop will be pulled out of the
// data and the option will be reformatted accordingly. Otherwise, the route
// option should be formatted as specified in RFC 791.
//

POption_Information=^TOption_Information;
TOption_Information=record
                     Ttl:byte;             // Time To Live
                     Tos:byte;             // Type Of Service
                     Flags:byte;           // IP header flags
                     OptionsSize:byte;     // Size in bytes of options data
                     OptionsData:pointer;  // Pointer to options data
                    end;



// The icmp_echo_reply structure describes the data returned in response
// to an echo request.
//

Picmp_echo_reply=^Ticmp_echo_reply;
Ticmp_echo_reply=record
                    Address:TipAddr;                // Replying address
                    Status:integer;                 // Reply IP_STATUS
                    RoundTripTime:integer;          // RTT in milliseconds
                    DataSize:word;                  // Reply data size in bytes
                    Reserved:word;                  // Reserved for system use
                    Data:pointer;                   // Pointer to the reply data
                    Options:Toption_Information;    // Reply options
                 end;
TsmICMP_Echo_Reply=record
                    Address:TipAddr;                // Replying address
                    Status:integer;                 // Reply IP_STATUS
                    RoundTripTime:integer;          // RTT in milliseconds
                    DataSize:word;                  // Reply data size in bytes
                    Reserved:word;                  // Reserved for system use
                    DataPtr:pointer;                // Pointer to the reply data
                    Options:Toption_Information;    // Reply options
                    Data: array[0..255] of Char;
                 end;

{

//++
//
// Routine Name:
//
//     IcmpCreateFile
//
// Routine Description:
//
//     Opens a handle on which ICMP Echo Requests can be issued.
//
// Arguments:
//
//     None.
//
// Return Value:
//
//     An open file handle or INVALID_HANDLE_VALUE. Extended error information
//     is available by calling GetLastError().
//
//--


function IcmpCreateFile:Thandle; StdCall;
// Routine Name:
//
//     IcmpCloseHandle
//
// Routine Description:
//
//     Closes a handle opened by ICMPOpenFile.
//
// Arguments:
//
//     IcmpHandle  - The handle to close.
//
// Return Value:
//
//     TRUE if the handle was closed successfully, otherwise FALSE. Extended
//     error information is available by calling GetLastError().
//
//--

function IcmpCloseHandle(H:Thandle):Bool; StdCall;

//
// Routine Name:
//
//     IcmpSendEcho
//
// Routine Description:
//
//     Sends an ICMP Echo request and returns any replies. The
//     call returns when the timeout has expired or the reply buffer
//     is filled.
//
// Arguments:
//
//     IcmpHandle           - An open handle returned by ICMPCreateFile.
//
//     DestinationAddress   - The destination of the echo request.
//
//     RequestData          - A buffer containing the data to send in the
//                            request.
//
//     RequestSize          - The number of bytes in the request data buffer.
//
//     RequestOptions       - Pointer to the IP header options for the request.
//                            May be NULL.
//
//     ReplyBuffer          - A buffer to hold any replies to the request.
//                            On return, the buffer will contain an array of
//                            ICMP_ECHO_REPLY structures followed by the
//                            options and data for the replies. The buffer
//                            should be large enough to hold at least one
//                            ICMP_ECHO_REPLY structure plus
//                            MAX(RequestSize, 8) bytes of data since an ICMP
//                            error message contains 8 bytes of data.
//
//     ReplySize            - The size in bytes of the reply buffer.
//
//     Timeout              - The time in milliseconds to wait for replies.
//
// Return Value:
//
//     Returns the number of ICMP_ECHO_REPLY structures stored in ReplyBuffer.
//     The status of each reply is contained in the structure. If the return
//     value is zero, extended error information is available via
//     GetLastError().
//
//--

Function IcmpSendEcho(IcmpHandle:Thandle;
                      DestinationAddress:TipAddr;
                      RequestData:pointer;
                      RequestSize:word;
                      RequestOptions:POption_Information;
                      ReplyBuffer:pointer;
                      ReplySize:integer;
                      Timeout:integer):Integer; stdcall;
}
Implementation
{
function IcmpCreateFile;        external 'Icmp.Dll';
function IcmpCloseHandle;       external 'Icmp.Dll';
Function IcmpSendEcho;          external 'Icmp.Dll';
 }
end.

