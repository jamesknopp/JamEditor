// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.BaseAncestorVCL.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_BaseancestorvclHPP
#define Virtualtrees_BaseancestorvclHPP

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
#include <Winapi.oleacc.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <VirtualTrees.Types.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Baseancestorvcl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTBaseAncestorVcl;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVTBaseAncestorVcl : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	_di_IAccessible FAccessible;
	_di_IAccessible FAccessibleItem;
	System::UnicodeString FAccessibleName;
	Vcl::Graphics::TBrush* FDottedBrushTreeLines;
	MESSAGE void __fastcall WMGetObject(Winapi::Messages::TMessage &Message);
	
protected:
	virtual HRESULT __fastcall DoRenderOLEData(const Winapi::Activex::TFormatEtc &FormatEtcIn, /* out */ Winapi::Activex::TStgMedium &Medium, bool ForClipboard) = 0 ;
	virtual HRESULT __fastcall RenderOLEData(const Winapi::Activex::TFormatEtc &FormatEtcIn, /* out */ Winapi::Activex::TStgMedium &Medium, bool ForClipboard);
	void __fastcall NotifyAccessibleEvent(unsigned pEvent = (unsigned)(0x800a));
	virtual Vcl::Graphics::TBrush* __fastcall PrepareDottedBrush(Vcl::Graphics::TBrush* CurrentDottedBrush, void * Bits, const System::Word BitsLinesCount);
	Vcl::Controls::TImageList* __fastcall CreateSystemImageSet();
	virtual void __fastcall SetWindowTheme(const System::UnicodeString Theme);
	virtual int __fastcall GetSelectedCount() = 0 ;
	virtual void __fastcall MarkCutCopyNodes() = 0 ;
	virtual void __fastcall DoStateChange(const Virtualtrees::Types::TVirtualTreeStates &Enter, const Virtualtrees::Types::TVirtualTreeStates &Leave = Virtualtrees::Types::TVirtualTreeStates() ) = 0 ;
	virtual Virtualtrees::Types::TNodeArray __fastcall GetSortedCutCopySet(bool Resolve) = 0 ;
	virtual Virtualtrees::Types::TNodeArray __fastcall GetSortedSelection(bool Resolve) = 0 ;
	virtual void __fastcall WriteNode(System::Classes::TStream* Stream, Virtualtrees::Types::PVirtualNode Node) = 0 ;
	virtual void __fastcall Sort(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TColumnIndex Column, Virtualtrees::Types::TSortDirection Direction, bool DoInit = true) = 0 ;
	virtual void __fastcall DoMouseEnter() = 0 ;
	virtual void __fastcall DoMouseLeave() = 0 ;
	__property Vcl::Graphics::TBrush* DottedBrushTreeLines = {read=FDottedBrushTreeLines, write=FDottedBrushTreeLines};
	
public:
	__fastcall virtual ~TVTBaseAncestorVcl();
	virtual void __fastcall CopyToClipboard();
	virtual void __fastcall CutToClipboard();
	virtual bool __fastcall PasteFromClipboard() = 0 ;
	System::LongBool __fastcall InvalidateRect(Winapi::Windows::PRect lpRect, System::LongBool bErase);
	System::LongBool __fastcall UpdateWindow();
	System::LongBool __fastcall RedrawWindow(Winapi::Windows::PRect lprcUpdate, HRGN hrgnUpdate, unsigned flags)/* overload */;
	System::LongBool __fastcall RedrawWindow(const Winapi::Windows::TRect &lprcUpdate, HRGN hrgnUpdate, unsigned flags)/* overload */;
	int __fastcall SendWM_SETREDRAW(bool Updating);
	void __fastcall ShowScrollBar(int Bar, bool AShow);
	Virtualtrees::Types::TDimension __fastcall SetScrollInfo(int Bar, const Winapi::Windows::TScrollInfo &ScrollInfo, bool Redraw);
	bool __fastcall GetScrollInfo(int Bar, Winapi::Windows::TScrollInfo &ScrollInfo);
	Virtualtrees::Types::TDimension __fastcall GetScrollPos(int Bar);
	System::LongBool __fastcall GetTextMetrics(Vcl::Graphics::TCanvas* Canvas, Winapi::Windows::TTextMetric &TM)/* overload */;
	__property _di_IAccessible Accessible = {read=FAccessible, write=FAccessible};
	__property _di_IAccessible AccessibleItem = {read=FAccessibleItem, write=FAccessibleItem};
	__property System::UnicodeString AccessibleName = {read=FAccessibleName, write=FAccessibleName};
public:
	/* TCustomControl.Create */ inline __fastcall virtual TVTBaseAncestorVcl(System::Classes::TComponent* AOwner) : Vcl::Controls::TCustomControl(AOwner) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVTBaseAncestorVcl(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Baseancestorvcl */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_BASEANCESTORVCL)
using namespace Virtualtrees::Baseancestorvcl;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_BaseancestorvclHPP
