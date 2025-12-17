// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.ClipBoard.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_ClipboardHPP
#define Virtualtrees_ClipboardHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>
#include <System.Classes.hpp>
#include <VirtualTrees.BaseTree.hpp>
#include <VirtualTrees.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Clipboard
{
//-- forward type declarations -----------------------------------------------
struct TClipboardFormatEntry;
class DELPHICLASS TClipboardFormatListEntry;
class DELPHICLASS TClipboardFormatList;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TClipboardFormatEntry
{
public:
	System::Word ID;
	System::UnicodeString Description;
};


typedef System::StaticArray<TClipboardFormatEntry, 17> Virtualtrees_Clipboard__1;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClipboardFormatListEntry : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Description;
	Virtualtrees::Basetree::TVirtualTreeClass TreeClass;
	unsigned Priority;
	Winapi::Activex::TFormatEtc FormatEtc;
public:
	/* TObject.Create */ inline __fastcall TClipboardFormatListEntry() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TClipboardFormatListEntry() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClipboardFormatList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static System::Classes::TList* __fastcall GetList();
	/* static */ __property System::Classes::TList* List = {read=GetList};
	
protected:
	__classmethod void __fastcall Sort();
	
public:
	__classmethod void __fastcall Add(const System::UnicodeString FormatString, Virtualtrees::Basetree::TVirtualTreeClass AClass, unsigned Priority, const Winapi::Activex::TFormatEtc &AFormatEtc);
	__classmethod void __fastcall Clear();
	__classmethod void __fastcall EnumerateFormats(Virtualtrees::Basetree::TVirtualTreeClass TreeClass, Virtualtrees::Basetree::TFormatEtcArray &Formats, Virtualtrees::Basetree::TClipboardFormats* const AllowedFormats = (Virtualtrees::Basetree::TClipboardFormats*)(0x0))/* overload */;
	__classmethod void __fastcall EnumerateFormats(Virtualtrees::Basetree::TVirtualTreeClass TreeClass, System::Classes::TStrings* const Formats)/* overload */;
	__classmethod TClipboardFormatListEntry* __fastcall FindFormat(const System::UnicodeString FormatString)/* overload */;
	__classmethod Virtualtrees::Basetree::TVirtualTreeClass __fastcall FindFormat(const System::UnicodeString FormatString, System::Word &Fmt)/* overload */;
	__classmethod Virtualtrees::Basetree::TVirtualTreeClass __fastcall FindFormat(System::Word Fmt, System::UnicodeString &Description)/* overload */;
public:
	/* TObject.Create */ inline __fastcall TClipboardFormatList() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TClipboardFormatList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Virtualtrees_Clipboard__1 ClipboardDescriptions;
extern DELPHI_PACKAGE System::Word CF_VIRTUALTREE;
extern DELPHI_PACKAGE System::Word CF_VTREFERENCE;
extern DELPHI_PACKAGE System::Word CF_VTHEADERREFERENCE;
extern DELPHI_PACKAGE System::Word CF_VRTF;
extern DELPHI_PACKAGE System::Word CF_VRTFNOOBJS;
extern DELPHI_PACKAGE System::Word CF_HTML;
extern DELPHI_PACKAGE System::Word CF_CSV;
extern DELPHI_PACKAGE void __fastcall EnumerateVTClipboardFormats(Virtualtrees::Basetree::TVirtualTreeClass TreeClass, System::Classes::TStrings* const List)/* overload */;
extern DELPHI_PACKAGE void __fastcall EnumerateVTClipboardFormats(Virtualtrees::Basetree::TVirtualTreeClass TreeClass, Virtualtrees::Basetree::TFormatEtcArray &Formats)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetVTClipboardFormatDescription(System::Word AFormat);
extern DELPHI_PACKAGE void __fastcall RegisterVTClipboardFormat(System::Word AFormat, Virtualtrees::Basetree::TVirtualTreeClass TreeClass, unsigned Priority)/* overload */;
extern DELPHI_PACKAGE System::Word __fastcall RegisterVTClipboardFormat(const System::UnicodeString Description, Virtualtrees::Basetree::TVirtualTreeClass TreeClass, unsigned Priority, int tymed = 0x1, Winapi::Activex::PDVTargetDevice ptd = (Winapi::Activex::PDVTargetDevice)(0x0), int dwAspect = 0x1, int lindex = 0xffffffff)/* overload */;
}	/* namespace Clipboard */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_CLIPBOARD)
using namespace Virtualtrees::Clipboard;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_ClipboardHPP
