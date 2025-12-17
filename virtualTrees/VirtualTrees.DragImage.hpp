// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.DragImage.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_DragimageHPP
#define Virtualtrees_DragimageHPP

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
#include <System.Types.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Dragimage
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTDragImage;
//-- type declarations -------------------------------------------------------
typedef System::Byte TVTTransparency;

enum DECLSPEC_DENUM TVTDragMoveRestriction : unsigned char { dmrNone, dmrHorizontalOnly, dmrVerticalOnly };

enum DECLSPEC_DENUM Virtualtrees_Dragimage__1 : unsigned char { disHidden, disInDrag, disPrepared };

typedef System::Set<Virtualtrees_Dragimage__1, Virtualtrees_Dragimage__1::disHidden, Virtualtrees_Dragimage__1::disPrepared> TVTDragImageStates;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDragImage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vcl::Controls::TCustomControl* FOwner;
	Vcl::Graphics::TBitmap* FBackImage;
	Vcl::Graphics::TBitmap* FAlphaImage;
	Vcl::Graphics::TBitmap* FDragImage;
	TVTDragMoveRestriction FRestriction;
	System::Uitypes::TColor FColorKey;
	TVTDragImageStates FStates;
	
public:
	__fastcall TVTDragImage(Vcl::Controls::TCustomControl* AOwner);
	__fastcall virtual ~TVTDragImage();
	void __fastcall EndDrag();
	void __fastcall PrepareDrag(Vcl::Graphics::TBitmap* DragImage, const System::Types::TPoint &HotSpot, const _di_IDataObject DataObject, System::Uitypes::TColor pColorKey = (System::Uitypes::TColor)(0xff000005));
	__property TVTDragMoveRestriction MoveRestriction = {read=FRestriction, write=FRestriction, default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dragimage */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_DRAGIMAGE)
using namespace Virtualtrees::Dragimage;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_DragimageHPP
