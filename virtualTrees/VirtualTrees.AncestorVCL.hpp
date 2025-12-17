// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.AncestorVCL.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_AncestorvclHPP
#define Virtualtrees_AncestorvclHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Themes.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.oleacc.hpp>
#include <Winapi.ActiveX.hpp>
#include <VirtualTrees.Types.hpp>
#include <VirtualTrees.BaseTree.hpp>
#include <VirtualTrees.BaseAncestorVCL.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Ancestorvcl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTAncestorVcl;
class DELPHICLASS TVirtualTreeHintWindow;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TVTRenderOLEDataEvent)(Virtualtrees::Basetree::TBaseVirtualTree* Sender, const Winapi::Activex::TFormatEtc &FormatEtcIn, /* out */ Winapi::Activex::TStgMedium &Medium, bool ForClipboard, HRESULT &Result);

class PASCALIMPLEMENTATION TVTAncestorVcl : public Virtualtrees::Basetree::TBaseVirtualTree
{
	typedef Virtualtrees::Basetree::TBaseVirtualTree inherited;
	
private:
	TVTRenderOLEDataEvent FOnRenderOLEData;
	
protected:
	virtual Vcl::Controls::THintWindowClass __fastcall GetHintWindowClass();
	__classmethod Virtualtrees::Basetree::TBaseVirtualTree* __fastcall GetTreeFromDataObject _DEPRECATED_ATTRIBUTE1("Use class TVTDragManager.GetTreeFromDataObject() instead") (const Virtualtrees::Types::TVTDragDataObject DataObject);
	virtual HRESULT __fastcall DoRenderOLEData(const Winapi::Activex::TFormatEtc &FormatEtcIn, /* out */ Winapi::Activex::TStgMedium &Medium, bool ForClipboard);
	__property TVTRenderOLEDataEvent OnRenderOLEData = {read=FOnRenderOLEData, write=FOnRenderOLEData};
	
public:
	virtual bool __fastcall PasteFromClipboard();
public:
	/* TBaseVirtualTree.Create */ inline __fastcall virtual TVTAncestorVcl(System::Classes::TComponent* AOwner) : Virtualtrees::Basetree::TBaseVirtualTree(AOwner) { }
	/* TBaseVirtualTree.Destroy */ inline __fastcall virtual ~TVTAncestorVcl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVTAncestorVcl(HWND ParentWindow) : Virtualtrees::Basetree::TBaseVirtualTree(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeHintWindow : public Vcl::Controls::THintWindow
{
	typedef Vcl::Controls::THintWindow inherited;
	
private:
	Virtualtrees::Basetree::TVTHintData FHintData;
	Virtualtrees::Basetree::TDimension FTextHeight;
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	
protected:
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall Paint();
	Vcl::Themes::TCustomStyleServices* __fastcall StyleServices(Vcl::Controls::TControl* AControl = (Vcl::Controls::TControl*)(0x0));
	
public:
	virtual Winapi::Windows::TRect __fastcall CalcHintRect(Virtualtrees::Basetree::TDimension MaxWidth, const System::UnicodeString AHint, void * AData);
	virtual bool __fastcall IsHintMsg(tagMSG &Msg);
public:
	/* THintWindow.Create */ inline __fastcall virtual TVirtualTreeHintWindow(System::Classes::TComponent* AOwner) : Vcl::Controls::THintWindow(AOwner) { }
	
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TVirtualTreeHintWindow() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualTreeHintWindow(HWND ParentWindow) : Vcl::Controls::THintWindow(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ancestorvcl */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_ANCESTORVCL)
using namespace Virtualtrees::Ancestorvcl;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_AncestorvclHPP
