// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.Colors.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_ColorsHPP
#define Virtualtrees_ColorsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Themes.hpp>
#include <Vcl.Controls.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Colors
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTColors;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTColors : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
	
private:
	enum DECLSPEC_DENUM TVTColorEnum : unsigned char { cDisabledColor, cDropMarkColor, cDropTargetColor, cFocusedSelectionColor, cGridLineColor, cTreeLineColor, cUnfocusedSelectionColor, cBorderColor, cHotColor, cFocusedSelectionBorderColor, cUnfocusedSelectionBorderColor, cDropTargetBorderColor, cSelectionRectangleBlendColor, cSelectionRectangleBorderColor, cHeaderHotColor, cSelectionTextColor, cUnfocusedColor };
	
	
private:
	static System::StaticArray<System::Uitypes::TColor, 17> cDefaultColors;
	Vcl::Controls::TCustomControl* FOwner;
	System::StaticArray<System::Uitypes::TColor, 17> FColors;
	System::Uitypes::TColor __fastcall GetColor(const TVTColorEnum Index);
	void __fastcall SetColor(const TVTColorEnum Index, const System::Uitypes::TColor Value);
	System::Uitypes::TColor __fastcall GetBackgroundColor();
	System::Uitypes::TColor __fastcall GetHeaderFontColor();
	System::Uitypes::TColor __fastcall GetNodeFontColor();
	
public:
	__fastcall TVTColors(Vcl::Controls::TCustomControl* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::Uitypes::TColor __fastcall GetSelectedNodeFontColor(bool Focused);
	__property System::Uitypes::TColor BackGroundColor = {read=GetBackgroundColor, nodefault};
	__property System::Uitypes::TColor HeaderFontColor = {read=GetHeaderFontColor, nodefault};
	__property System::Uitypes::TColor NodeFontColor = {read=GetNodeFontColor, nodefault};
	Vcl::Themes::TCustomStyleServices* __fastcall StyleServices(Vcl::Controls::TControl* AControl = (Vcl::Controls::TControl*)(0x0));
	
__published:
	__property System::Uitypes::TColor BorderColor = {read=GetColor, write=SetColor, index=7, default=-16777201};
	__property System::Uitypes::TColor DisabledColor = {read=GetColor, write=SetColor, index=0, default=-16777200};
	__property System::Uitypes::TColor DropMarkColor = {read=GetColor, write=SetColor, index=1, default=-16777203};
	__property System::Uitypes::TColor DropTargetColor = {read=GetColor, write=SetColor, index=2, default=-16777203};
	__property System::Uitypes::TColor DropTargetBorderColor = {read=GetColor, write=SetColor, index=11, default=-16777203};
	__property System::Uitypes::TColor FocusedSelectionColor = {read=GetColor, write=SetColor, index=3, default=-16777203};
	__property System::Uitypes::TColor FocusedSelectionBorderColor = {read=GetColor, write=SetColor, index=9, default=-16777203};
	__property System::Uitypes::TColor GridLineColor = {read=GetColor, write=SetColor, index=4, default=-16777201};
	__property System::Uitypes::TColor HeaderHotColor = {read=GetColor, write=SetColor, index=14, default=-16777200};
	__property System::Uitypes::TColor HotColor = {read=GetColor, write=SetColor, index=8, default=-16777208};
	__property System::Uitypes::TColor SelectionRectangleBlendColor = {read=GetColor, write=SetColor, index=12, default=-16777203};
	__property System::Uitypes::TColor SelectionRectangleBorderColor = {read=GetColor, write=SetColor, index=13, default=-16777203};
	__property System::Uitypes::TColor SelectionTextColor = {read=GetColor, write=SetColor, index=15, default=-16777202};
	__property System::Uitypes::TColor TreeLineColor = {read=GetColor, write=SetColor, index=5, default=-16777200};
	__property System::Uitypes::TColor UnfocusedColor = {read=GetColor, write=SetColor, index=16, default=-16777197};
	__property System::Uitypes::TColor UnfocusedSelectionColor = {read=GetColor, write=SetColor, index=6, default=-16777213};
	__property System::Uitypes::TColor UnfocusedSelectionBorderColor = {read=GetColor, write=SetColor, index=10, default=-16777213};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TVTColors() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Colors */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_COLORS)
using namespace Virtualtrees::Colors;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_ColorsHPP
