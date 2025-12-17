// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.BaseTree.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_BasetreeHPP
#define Virtualtrees_BasetreeHPP

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
#include <Winapi.Messages.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.CommCtrl.hpp>
#include <Winapi.UxTheme.hpp>
#include <Winapi.ShlObj.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Printers.hpp>
#include <Vcl.Themes.hpp>
#include <System.UITypes.hpp>
#include <VirtualTrees.Types.hpp>
#include <VirtualTrees.Colors.hpp>
#include <VirtualTrees.DragImage.hpp>
#include <VirtualTrees.Header.hpp>
#include <VirtualTrees.BaseAncestorVCL.hpp>

//-- user supplied -----------------------------------------------------------
#include <objidl.h>
#include <oleidl.h>
#include <oleacc.h>
#pragma comment(lib, "VirtualTreesR")
#pragma comment(lib, "Shell32")
#pragma comment(lib, "uxtheme")
#pragma link "VirtualTrees.Accessibility"

namespace Virtualtrees
{
namespace Basetree
{
//-- forward type declarations -----------------------------------------------
struct TCacheEntry;
struct TVTReference;
__interface DELPHIINTERFACE IVTDragManager;
typedef System::DelphiInterface<IVTDragManager> _di_IVTDragManager;
struct TVTHintData;
__interface DELPHIINTERFACE IVTEditLink;
typedef System::DelphiInterface<IVTEditLink> _di_IVTEditLink;
class DELPHICLASS TClipboardFormats;
__interface DELPHIINTERFACE TVTGetNodeProc;
typedef System::DelphiInterface<TVTGetNodeProc> _di_TVTGetNodeProc;
struct TVTVirtualNodeEnumerator;
struct TVTVirtualNodeEnumeration;
class DELPHICLASS TBaseVirtualTree;
//-- type declarations -------------------------------------------------------
typedef Virtualtrees::Baseancestorvcl::TVTBaseAncestorVcl TVTBaseAncestor;

using Vcl::Graphics::TCanvas;

using Virtualtrees::Types::TFormatEtcArray;

using System::Uitypes::TImageIndex;

using Virtualtrees::Types::TDimension;

using Virtualtrees::Types::TColumnIndex;

using Virtualtrees::Types::TColumnPosition;

using Virtualtrees::Types::EVirtualTreeError;

using Virtualtrees::Types::TAutoScrollInterval;

using Virtualtrees::Types::TVTScrollIncrement;

using Virtualtrees::Types::TFormatArray;

using Virtualtrees::Types::TVTPaintOption;

using Virtualtrees::Types::TVTPaintOptions;

typedef Virtualtrees::Types::TVTAnimationOption TVTAnimateOption;

typedef Virtualtrees::Types::TVTAnimationOptions TVTAnimateOptions;

using Virtualtrees::Types::TVTAutoOption;

using Virtualtrees::Types::TVTAutoOptions;

using Virtualtrees::Types::TVTSelectionOption;

using Virtualtrees::Types::TVTSelectionOptions;

using Virtualtrees::Types::TVTEditOptions;

using Virtualtrees::Types::TVTMiscOption;

using Virtualtrees::Types::TVTMiscOptions;

using Virtualtrees::Types::TVTExportMode;

using Virtualtrees::Types::TVTStringOption;

using Virtualtrees::Types::TVTStringOptions;

using Virtualtrees::Types::TCustomVirtualTreeOptions;

using Virtualtrees::Types::TVirtualTreeOptions;

using Virtualtrees::Types::TTreeOptionsClass;

using Virtualtrees::Types::TCustomStringTreeOptions;

using Virtualtrees::Types::TStringTreeOptions;

using Virtualtrees::Types::TScrollBarStyle;

using Virtualtrees::Types::TScrollBarOptions;

using Virtualtrees::Types::TVTColumnOption;

using Virtualtrees::Types::TVTColumnOptions;

using Virtualtrees::Types::TVirtualTreeColumnStyle;

using Virtualtrees::Types::TSortDirection;

using Virtualtrees::Types::TCheckType;

using Virtualtrees::Types::TCheckState;

using Virtualtrees::Types::TVTDropMarkMode;

using Virtualtrees::Types::TScrollDirections;

using Virtualtrees::Header::TVirtualTreeColumn;

using Virtualtrees::Header::TVirtualTreeColumns;

using Virtualtrees::Header::TVirtualTreeColumnClass;

using Virtualtrees::Header::TColumnsArray;

using Virtualtrees::Header::TCardinalArray;

using Virtualtrees::Header::TIndexArray;

using Virtualtrees::Colors::TVTColors;

typedef System::TMetaClass* TVirtualTreeClass;

struct DECLSPEC_DRECORD TCacheEntry
{
public:
	Virtualtrees::Types::PVirtualNode Node;
	Virtualtrees::Types::TNodeHeight AbsoluteTop;
};


typedef System::DynamicArray<TCacheEntry> TCache;

typedef TVTReference *PVTReference;

struct DECLSPEC_DRECORD TVTReference
{
public:
	unsigned Process;
	TBaseVirtualTree* Tree;
};


__interface  INTERFACE_UUID("{C4B25559-14DA-446B-8901-0C879000EB16}") IVTDragManager  : public System::IInterface 
{
	virtual void __stdcall ForceDragLeave() = 0 ;
	virtual Virtualtrees::Types::IDataObject __stdcall GetDataObject() = 0 ;
	virtual TBaseVirtualTree* __stdcall GetDragSource() = 0 ;
	virtual bool __stdcall GetIsDropTarget() = 0 ;
	__property Virtualtrees::Types::IDataObject DataObject = {read=GetDataObject};
	__property TBaseVirtualTree* DragSource = {read=GetDragSource};
	__property bool IsDropTarget = {read=GetIsDropTarget};
};

typedef TVTHintData *PVTHintData;

struct DECLSPEC_DRECORD TVTHintData
{
public:
	TBaseVirtualTree* Tree;
	Virtualtrees::Types::PVirtualNode Node;
	TColumnIndex Column;
	System::Types::TRect HintRect;
	System::UnicodeString HintText;
	System::Classes::TBiDiMode BidiMode;
	System::Classes::TAlignment Alignment;
	Virtualtrees::Types::TVTTooltipLineBreakStyle LineBreakStyle;
};


__interface  INTERFACE_UUID("{2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}") IVTEditLink  : public System::IInterface 
{
	virtual bool __stdcall BeginEdit() = 0 ;
	virtual bool __stdcall CancelEdit() = 0 ;
	virtual bool __stdcall EndEdit() = 0 ;
	virtual bool __stdcall PrepareEdit(TBaseVirtualTree* Tree, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column) = 0 ;
	virtual void __stdcall ProcessMessage(Winapi::Messages::TMessage &Message) = 0 ;
	virtual void __stdcall SetBounds(System::Types::TRect R) = 0 ;
};

typedef void __fastcall (__closure *TVTNodeExportEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::TVTExportType aExportType, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTColumnExportEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::TVTExportType aExportType, TVirtualTreeColumn* Column);

typedef void __fastcall (__closure *TVTTreeExportEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::TVTExportType aExportType);

class PASCALIMPLEMENTATION TClipboardFormats : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TBaseVirtualTree* FOwner;
	
public:
	__fastcall virtual TClipboardFormats(TBaseVirtualTree* AOwner);
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S);
	__property TBaseVirtualTree* Owner = {read=FOwner};
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TClipboardFormats() { }
	
};


__interface TVTGetNodeProc  : public System::IInterface 
{
	virtual void __fastcall Invoke(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, void * Data, bool &Abort) = 0 ;
};

typedef void __fastcall (__closure *TVTChangingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, bool &Allowed);

typedef void __fastcall (__closure *TVTCheckChangingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TCheckState &NewState, bool &Allowed);

typedef void __fastcall (__closure *TVTChangeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTStructureChangeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TChangeReason Reason);

typedef void __fastcall (__closure *TVTEditCancelEvent)(TBaseVirtualTree* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTEditChangingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTEditChangeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);

typedef void __fastcall (__closure *TVTFreeNodeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTFocusChangingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode OldNode, Virtualtrees::Types::PVirtualNode NewNode, TColumnIndex OldColumn, TColumnIndex NewColumn, bool &Allowed);

typedef void __fastcall (__closure *TVTFocusChangeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);

typedef void __fastcall (__closure *TVTAddToSelectionEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTRemoveFromSelectionEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTGetImageEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind, TColumnIndex Column, bool &Ghosted, TImageIndex &ImageIndex);

typedef void __fastcall (__closure *TVTGetImageExEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind, TColumnIndex Column, bool &Ghosted, TImageIndex &ImageIndex, Vcl::Imglist::TCustomImageList* &ImageList);

typedef void __fastcall (__closure *TVTGetImageTextEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind, TColumnIndex Column, System::UnicodeString &ImageText);

typedef void __fastcall (__closure *TVTHotNodeChangeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode OldNode, Virtualtrees::Types::PVirtualNode NewNode);

typedef void __fastcall (__closure *TVTInitChildrenEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, unsigned &ChildCount);

typedef void __fastcall (__closure *TVTInitNodeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode ParentNode, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVirtualNodeInitStates &InitialStates);

typedef void __fastcall (__closure *TVTPopupEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &P, bool &AskParent, Vcl::Menus::TPopupMenu* &PopupMenu);

typedef void __fastcall (__closure *TVTHelpContextEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, int &HelpContext);

typedef void __fastcall (__closure *TVTCreateEditorEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, /* out */ _di_IVTEditLink &EditLink);

typedef void __fastcall (__closure *TVTSaveTreeEvent)(TBaseVirtualTree* Sender, System::Classes::TStream* Stream);

typedef void __fastcall (__closure *TVTSaveNodeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, System::Classes::TStream* Stream);

typedef void __fastcall (__closure *TVTBeforeGetCheckStateEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTHeaderAddPopupItemEvent)(System::TObject* const Sender, const TColumnIndex Column, Virtualtrees::Types::TAddPopupItemType &Cmd);

typedef void __fastcall (__closure *TVTHeaderClickEvent)(Virtualtrees::Header::TVTHeader* Sender, const Virtualtrees::Types::TVTHeaderHitInfo &HitInfo);

typedef void __fastcall (__closure *TVTHeaderMouseEvent)(Virtualtrees::Header::TVTHeader* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, TDimension X, TDimension Y);

typedef void __fastcall (__closure *TVTHeaderMouseMoveEvent)(Virtualtrees::Header::TVTHeader* Sender, System::Classes::TShiftState Shift, TDimension X, TDimension Y);

typedef void __fastcall (__closure *TVTBeforeHeaderHeightTrackingEvent)(Virtualtrees::Header::TVTHeader* Sender, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTAfterHeaderHeightTrackingEvent)(Virtualtrees::Header::TVTHeader* Sender);

typedef void __fastcall (__closure *TVTHeaderHeightTrackingEvent)(Virtualtrees::Header::TVTHeader* Sender, System::Types::TPoint &P, System::Classes::TShiftState Shift, bool &Allowed);

typedef void __fastcall (__closure *TVTHeaderHeightDblClickResizeEvent)(Virtualtrees::Header::TVTHeader* Sender, System::Types::TPoint &P, System::Classes::TShiftState Shift, bool &Allowed);

typedef void __fastcall (__closure *TVTHeaderNotifyEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTHeaderDraggingEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTHeaderDraggedEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, int OldPosition);

typedef void __fastcall (__closure *TVTHeaderDraggedOutEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, const System::Types::TPoint &DropPosition);

typedef void __fastcall (__closure *TVTHeaderPaintEvent)(Virtualtrees::Header::TVTHeader* Sender, TCanvas* HeaderCanvas, TVirtualTreeColumn* Column, const System::Types::TRect &R, bool Hover, bool Pressed, TVTDropMarkMode DropMark);

typedef void __fastcall (__closure *TVTHeaderPaintQueryElementsEvent)(Virtualtrees::Header::TVTHeader* Sender, Virtualtrees::Header::THeaderPaintInfo &PaintInfo, Virtualtrees::Types::THeaderPaintElements &Elements);

typedef void __fastcall (__closure *TVTAdvancedHeaderPaintEvent)(Virtualtrees::Header::TVTHeader* Sender, Virtualtrees::Header::THeaderPaintInfo &PaintInfo, const Virtualtrees::Types::THeaderPaintElements Elements);

typedef void __fastcall (__closure *TVTBeforeAutoFitColumnsEvent)(Virtualtrees::Header::TVTHeader* Sender, Virtualtrees::Types::TSmartAutoFitType &SmartAutoFitType);

typedef void __fastcall (__closure *TVTBeforeAutoFitColumnEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, Virtualtrees::Types::TSmartAutoFitType &SmartAutoFitType, bool &Allowed);

typedef void __fastcall (__closure *TVTAfterAutoFitColumnEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTAfterAutoFitColumnsEvent)(Virtualtrees::Header::TVTHeader* Sender);

typedef void __fastcall (__closure *TVTColumnCheckChangingEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, TCheckState &NewState, bool &Allowed);

typedef void __fastcall (__closure *TVTColumnClickEvent)(TBaseVirtualTree* Sender, TColumnIndex Column, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTColumnDblClickEvent)(TBaseVirtualTree* Sender, TColumnIndex Column, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TColumnChangeEvent)(TBaseVirtualTree* const Sender, const TColumnIndex Column, bool Visible);

typedef void __fastcall (__closure *TVTColumnWidthDblClickResizeEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, System::Classes::TShiftState Shift, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTBeforeColumnWidthTrackingEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTAfterColumnWidthTrackingEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTColumnWidthTrackingEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTGetHeaderCursorEvent)(Virtualtrees::Header::TVTHeader* Sender, Virtualtrees::Types::TVTCursor &Cursor);

typedef void __fastcall (__closure *TVTBeforeGetMaxColumnWidthEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, bool &UseSmartColumnWidth);

typedef void __fastcall (__closure *TVTAfterGetMaxColumnWidthEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, TDimension &MaxWidth);

typedef void __fastcall (__closure *TVTCanSplitterResizeColumnEvent)(Virtualtrees::Header::TVTHeader* Sender, const System::Types::TPoint &P, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTCanSplitterResizeHeaderEvent)(Virtualtrees::Header::TVTHeader* Sender, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeMovedEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTNodeMovingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::PVirtualNode Target, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeCopiedEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node);

typedef void __fastcall (__closure *TVTNodeCopyingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::PVirtualNode Target, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeClickEvent)(TBaseVirtualTree* Sender, const Virtualtrees::Types::THitInfo &HitInfo);

typedef void __fastcall (__closure *TVTNodeHeightTrackingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeHeightDblClickResizeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTCanSplitterResizeNodeEvent)(TBaseVirtualTree* Sender, const System::Types::TPoint &P, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTGetUserClipboardFormatsEvent)(TBaseVirtualTree* Sender, TFormatEtcArray &Formats);

typedef void __fastcall (__closure *TVTCreateDragManagerEvent)(TBaseVirtualTree* Sender, /* out */ _di_IVTDragManager &DragManager);

typedef void __fastcall (__closure *TVTCreateDataObjectEvent)(TBaseVirtualTree* Sender, /* out */ Virtualtrees::Types::TVTDragDataObject &IDataObject);

typedef void __fastcall (__closure *TVTDragAllowedEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTDragOverEvent)(TBaseVirtualTree* Sender, System::TObject* Source, System::Classes::TShiftState Shift, System::Uitypes::TDragState State, const System::Types::TPoint &Pt, Virtualtrees::Types::TDropMode Mode, int &Effect, bool &Accept);

typedef void __fastcall (__closure *TVTDragDropEvent)(TBaseVirtualTree* Sender, System::TObject* Source, Virtualtrees::Types::TVTDragDataObject DataObject, TFormatArray Formats, System::Classes::TShiftState Shift, const System::Types::TPoint &Pt, int &Effect, Virtualtrees::Types::TDropMode Mode);

typedef void __fastcall (__closure *TVTBeforeItemEraseEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect, System::Uitypes::TColor &ItemColor, Virtualtrees::Types::TItemEraseAction &EraseAction);

typedef void __fastcall (__closure *TVTAfterItemEraseEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect);

typedef void __fastcall (__closure *TVTBeforeItemPaintEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect, bool &CustomDraw);

typedef void __fastcall (__closure *TVTAfterItemPaintEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect);

typedef void __fastcall (__closure *TVTBeforeCellPaintEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVTCellPaintMode CellPaintMode, const System::Types::TRect &CellRect, System::Types::TRect &ContentRect);

typedef void __fastcall (__closure *TVTAfterCellPaintEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TRect &CellRect);

typedef void __fastcall (__closure *TVTPaintEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas);

typedef void __fastcall (__closure *TVTBackgroundPaintEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, const System::Types::TRect &R, bool &Handled);

typedef void __fastcall (__closure *TVTGetLineStyleEvent)(TBaseVirtualTree* Sender, void * &Bits);

typedef void __fastcall (__closure *TVTMeasureItemEvent)(TBaseVirtualTree* Sender, TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, TDimension &NodeHeight);

typedef void __fastcall (__closure *TVTPaintText)(TBaseVirtualTree* Sender, TCanvas* const TargetCanvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVSTTextType TextType);

typedef void __fastcall (__closure *TVTPrepareButtonImagesEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TBitmap* const APlusBM, Vcl::Graphics::TBitmap* const APlusHotBM, Vcl::Graphics::TBitmap* const APlusSelectedHotBM, Vcl::Graphics::TBitmap* const AMinusBM, Vcl::Graphics::TBitmap* const AMinusHotBM, Vcl::Graphics::TBitmap* const AMinusSelectedHotBM, System::Types::TSize &ASize);

typedef void __fastcall (__closure *TVTColumnHeaderSpanningEvent)(Virtualtrees::Header::TVTHeader* Sender, TColumnIndex Column, int &Count);

typedef void __fastcall (__closure *TVTCompareEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node1, Virtualtrees::Types::PVirtualNode Node2, TColumnIndex Column, int &Result);

typedef void __fastcall (__closure *TVTIncrementalSearchEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, const System::UnicodeString SearchText, int &Result);

typedef void __fastcall (__closure *TVTOperationEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::TVTOperationKind OperationKind);

enum DECLSPEC_DENUM TVTHintKind : unsigned char { vhkText, vhkOwnerDraw };

typedef void __fastcall (__closure *TVTHintKindEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, TVTHintKind &Kind);

typedef void __fastcall (__closure *TVTDrawHintEvent)(TBaseVirtualTree* Sender, TCanvas* HintCanvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &R, TColumnIndex Column);

typedef void __fastcall (__closure *TVTGetHintSizeEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, System::Types::TRect &R);

typedef void __fastcall (__closure *TVTBeforeDrawLineImageEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, int Level, TDimension &PosX);

typedef void __fastcall (__closure *TVTGetNodeDataSizeEvent)(TBaseVirtualTree* Sender, int &NodeDataSize);

typedef void __fastcall (__closure *TVTKeyActionEvent)(TBaseVirtualTree* Sender, System::Word &CharCode, System::Classes::TShiftState &Shift, bool &DoDefault);

typedef void __fastcall (__closure *TVTScrollEvent)(TBaseVirtualTree* Sender, TDimension DeltaX, TDimension DeltaY);

typedef void __fastcall (__closure *TVTUpdatingEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::TVTUpdateState State);

typedef void __fastcall (__closure *TVTGetCursorEvent)(TBaseVirtualTree* Sender, System::Uitypes::TCursor &Cursor);

typedef void __fastcall (__closure *TVTStateChangeEvent)(TBaseVirtualTree* Sender, const Virtualtrees::Types::TVirtualTreeStates &Enter, const Virtualtrees::Types::TVirtualTreeStates &Leave);

typedef void __fastcall (__closure *TVTGetCellIsEmptyEvent)(TBaseVirtualTree* Sender, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool &IsEmpty);

typedef void __fastcall (__closure *TVTScrollBarShowEvent)(TBaseVirtualTree* Sender, int Bar, bool Show);

typedef Virtualtrees::Types::PVirtualNode __fastcall (__closure *TGetFirstNodeProc)(void);

typedef Virtualtrees::Types::PVirtualNode __fastcall (__closure *TGetNextNodeProc)(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove/* = false*/);

enum DECLSPEC_DENUM TVZVirtualNodeEnumerationMode : unsigned char { vneAll, vneChecked, vneChild, vneCutCopy, vneInitialized, vneLeaf, vneLevel, vneNoInit, vneSelected, vneVisible, vneVisibleChild, vneVisibleNoInitChild, vneVisibleNoInit };

typedef TVTVirtualNodeEnumeration *PVTVirtualNodeEnumeration;

struct DECLSPEC_DRECORD TVTVirtualNodeEnumerator
{
private:
	Virtualtrees::Types::PVirtualNode FNode;
	bool FCanMoveNext;
	PVTVirtualNodeEnumeration FEnumeration;
	Virtualtrees::Types::PVirtualNode __fastcall GetCurrent();
	
public:
	bool __fastcall MoveNext();
	__property Virtualtrees::Types::PVirtualNode Current = {read=GetCurrent};
};


struct DECLSPEC_DRECORD TVTVirtualNodeEnumeration
{
private:
	TVZVirtualNodeEnumerationMode FMode;
	TBaseVirtualTree* FTree;
	bool FConsiderChildrenAbove;
	Virtualtrees::Types::PVirtualNode FNode;
	unsigned FNodeLevel;
	TCheckState FState;
	bool FIncludeFiltered;
	
public:
	TVTVirtualNodeEnumerator __fastcall GetEnumerator();
	
private:
	Virtualtrees::Types::PVirtualNode __fastcall GetNext(Virtualtrees::Types::PVirtualNode Node);
};


class PASCALIMPLEMENTATION TBaseVirtualTree : public Virtualtrees::Baseancestorvcl::TVTBaseAncestorVcl
{
	typedef Virtualtrees::Baseancestorvcl::TVTBaseAncestorVcl inherited;
	
private:
	unsigned FTotalInternalDataSize;
	Vcl::Forms::TBorderStyle FBorderStyle;
	Virtualtrees::Header::TVTHeader* FHeader;
	Virtualtrees::Types::PVirtualNode FRoot;
	TDimension FDefaultNodeHeight;
	TDimension FIndent;
	TCustomVirtualTreeOptions* FOptions;
	unsigned FUpdateCount;
	unsigned FSynchUpdateCount;
	int FNodeDataSize;
	Virtualtrees::Types::TVirtualTreeStates FStates;
	Virtualtrees::Types::PVirtualNode FLastSelected;
	Virtualtrees::Types::PVirtualNode FFocusedNode;
	TColumnIndex FEditColumn;
	TColumnIndex FFocusedColumn;
	System::Types::TPoint FHeightTrackPoint;
	Virtualtrees::Types::PVirtualNode FHeightTrackNode;
	TColumnIndex FHeightTrackColumn;
	TScrollDirections FScrollDirections;
	Virtualtrees::Types::TChangeReason FLastStructureChangeReason;
	Virtualtrees::Types::PVirtualNode FLastStructureChangeNode;
	Virtualtrees::Types::PVirtualNode FLastChangedNode;
	Virtualtrees::Types::PVirtualNode FCurrentHotNode;
	TColumnIndex FCurrentHotColumn;
	bool FHotNodeButtonHit;
	System::Types::TRect FLastSelRect;
	System::Types::TRect FNewSelRect;
	System::Uitypes::TCursor FHotCursor;
	Virtualtrees::Types::THitInfo FLastHitInfo;
	Virtualtrees::Types::TVTHintMode FHintMode;
	TVTHintData FHintData;
	unsigned FChangeDelay;
	unsigned FEditDelay;
	TCache FPositionCache;
	unsigned FVisibleCount;
	unsigned FStartIndex;
	Virtualtrees::Types::TNodeArray FSelection;
	bool FSelectionLocked;
	Virtualtrees::Types::PVirtualNode FRangeAnchor;
	unsigned FCheckPropagationCount;
	int FLastSelectionLevel;
	System::Classes::TShiftState FDrawSelShiftState;
	_di_IVTEditLink FEditLink;
	Virtualtrees::Types::TNodeArray FTempNodeCache;
	unsigned FTempNodeCount;
	Virtualtrees::Types::TVTBackground* FBackground;
	bool FBackgroundImageTransparent;
	TDimension FMargin;
	TDimension FTextMargin;
	TDimension FBackgroundOffsetX;
	TDimension FBackgroundOffsetY;
	unsigned FAnimationDuration;
	bool FWantTabs;
	Virtualtrees::Types::TVTNodeAlignment FNodeAlignment;
	System::Types::TRect FHeaderRect;
	System::Types::TRect FLastHintRect;
	System::Types::TRect FUpdateRect;
	System::UnicodeString FEmptyListMessage;
	Vcl::Graphics::TBitmap* FPlusBM;
	Vcl::Graphics::TBitmap* FMinusBM;
	Vcl::Graphics::TBitmap* FHotPlusBM;
	Vcl::Graphics::TBitmap* FHotMinusBM;
	Vcl::Graphics::TBitmap* FSelectedHotPlusBM;
	Vcl::Graphics::TBitmap* FSelectedHotMinusBM;
	Vcl::Imglist::TCustomImageList* FImages;
	Vcl::Imglist::TCustomImageList* FStateImages;
	Vcl::Imglist::TCustomImageList* FCustomCheckImages;
	Virtualtrees::Types::TCheckImageKind FCheckImageKind;
	Vcl::Imglist::TCustomImageList* FCheckImages;
	TDimension FImagesMargin;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	Vcl::Imglist::TChangeLink* FStateChangeLink;
	Vcl::Imglist::TChangeLink* FCustomCheckChangeLink;
	System::Classes::TNotifyEvent FOldFontChange;
	TVTColors* FColors;
	Virtualtrees::Types::TVTButtonStyle FButtonStyle;
	Virtualtrees::Types::TVTButtonFillMode FButtonFillMode;
	Virtualtrees::Types::TVTLineStyle FLineStyle;
	Virtualtrees::Types::TVTLineMode FLineMode;
	unsigned FSelectionCurveRadius;
	System::Byte FSelectionBlendFactor;
	Virtualtrees::Types::TVTDrawSelectionMode FDrawSelectionMode;
	System::Classes::TAlignment FAlignment;
	Virtualtrees::Types::TVTDragImageKind FDragImageKind;
	Virtualtrees::Types::TDragOperations FDragOperations;
	int FDragThreshold;
	_di_IVTDragManager FDragManager;
	Virtualtrees::Types::PVirtualNode FDropTargetNode;
	Virtualtrees::Types::TDropMode FLastDropMode;
	Virtualtrees::Types::TNodeArray FDragSelection;
	int FLastDragEffect;
	Virtualtrees::Types::TVTDragType FDragType;
	int FDragWidth;
	int FDragHeight;
	TClipboardFormats* FClipboardFormats;
	Virtualtrees::Types::PVirtualNode FLastVCLDragTarget;
	int FVCLDragEffect;
	TScrollBarOptions* FScrollBarOptions;
	TAutoScrollInterval FAutoScrollInterval;
	unsigned FAutoScrollDelay;
	unsigned FAutoExpandDelay;
	TDimension FOffsetX;
	TDimension FOffsetY;
	TDimension FEffectiveOffsetX;
	Virtualtrees::Types::TNodeHeight FRangeX;
	Virtualtrees::Types::TNodeHeight FRangeY;
	TDimension FBottomSpace;
	Virtualtrees::Types::TVTNodeAttachMode FDefaultPasteMode;
	unsigned FDragScrollStart;
	Virtualtrees::Types::TVTIncrementalSearch FIncrementalSearch;
	unsigned FSearchTimeout;
	System::UnicodeString FSearchBuffer;
	Virtualtrees::Types::PVirtualNode FLastSearchNode;
	Virtualtrees::Types::TVTSearchDirection FSearchDirection;
	Virtualtrees::Types::TVTSearchStart FSearchStart;
	Vcl::Forms::TForm* FPanningWindow;
	Virtualtrees::Types::TVTCursor FPanningCursor;
	System::Types::TPoint FLastClickPos;
	unsigned FOperationCount;
	bool FOperationCanceled;
	bool FChangingTheme;
	Virtualtrees::Types::PVirtualNode FNextNodeToSelect;
	int FPendingSyncProcs;
	TVTNodeExportEvent FOnBeforeNodeExport;
	TVTNodeExportEvent FOnNodeExport;
	TVTNodeExportEvent FOnAfterNodeExport;
	TVTColumnExportEvent FOnBeforeColumnExport;
	TVTColumnExportEvent FOnColumnExport;
	TVTColumnExportEvent FOnAfterColumnExport;
	TVTTreeExportEvent FOnBeforeTreeExport;
	TVTTreeExportEvent FOnAfterTreeExport;
	TVTTreeExportEvent FOnBeforeHeaderExport;
	TVTTreeExportEvent FOnAfterHeaderExport;
	TVTChangeEvent FOnChange;
	TVTStructureChangeEvent FOnStructureChange;
	TVTInitChildrenEvent FOnInitChildren;
	TVTInitNodeEvent FOnInitNode;
	TVTFreeNodeEvent FOnFreeNode;
	TVTGetImageEvent FOnGetImage;
	TVTGetImageExEvent FOnGetImageEx;
	TVTGetImageTextEvent FOnGetImageText;
	TVTHotNodeChangeEvent FOnHotChange;
	TVTChangingEvent FOnExpanding;
	TVTChangingEvent FOnCollapsing;
	TVTCheckChangingEvent FOnChecking;
	TVTChangeEvent FOnExpanded;
	TVTChangeEvent FOnCollapsed;
	TVTChangeEvent FOnChecked;
	TVTChangeEvent FOnResetNode;
	TVTNodeMovingEvent FOnNodeMoving;
	TVTNodeMovedEvent FOnNodeMoved;
	TVTNodeCopyingEvent FOnNodeCopying;
	TVTNodeClickEvent FOnNodeClick;
	TVTNodeClickEvent FOnNodeDblClick;
	TVTCanSplitterResizeNodeEvent FOnCanSplitterResizeNode;
	TVTNodeHeightTrackingEvent FOnNodeHeightTracking;
	TVTNodeHeightDblClickResizeEvent FOnNodeHeightDblClickResize;
	TVTNodeCopiedEvent FOnNodeCopied;
	TVTEditChangingEvent FOnEditing;
	TVTEditCancelEvent FOnEditCancelled;
	TVTEditChangeEvent FOnEdited;
	TVTFocusChangingEvent FOnFocusChanging;
	TVTFocusChangeEvent FOnFocusChanged;
	TVTAddToSelectionEvent FOnAddToSelection;
	TVTRemoveFromSelectionEvent FOnRemoveFromSelection;
	TVTPopupEvent FOnGetPopupMenu;
	TVTHelpContextEvent FOnGetHelpContext;
	TVTCreateEditorEvent FOnCreateEditor;
	TVTSaveNodeEvent FOnLoadNode;
	TVTSaveNodeEvent FOnSaveNode;
	TVTSaveTreeEvent FOnLoadTree;
	TVTSaveTreeEvent FOnSaveTree;
	TVTAfterAutoFitColumnEvent FOnAfterAutoFitColumn;
	TVTAfterAutoFitColumnsEvent FOnAfterAutoFitColumns;
	TVTBeforeAutoFitColumnsEvent FOnBeforeAutoFitColumns;
	TVTBeforeAutoFitColumnEvent FOnBeforeAutoFitColumn;
	TVTHeaderAddPopupItemEvent FOnHeaderAddPopupItem;
	TVTHeaderClickEvent FOnHeaderClick;
	TVTHeaderClickEvent FOnHeaderDblClick;
	TVTAfterHeaderHeightTrackingEvent FOnAfterHeaderHeightTracking;
	TVTBeforeHeaderHeightTrackingEvent FOnBeforeHeaderHeightTracking;
	TVTHeaderHeightTrackingEvent FOnHeaderHeightTracking;
	TVTHeaderHeightDblClickResizeEvent FOnHeaderHeightDblClickResize;
	TVTHeaderMouseEvent FOnHeaderMouseDown;
	TVTHeaderMouseEvent FOnHeaderMouseUp;
	TVTHeaderMouseMoveEvent FOnHeaderMouseMove;
	TVTAfterGetMaxColumnWidthEvent FOnAfterGetMaxColumnWidth;
	TVTBeforeGetMaxColumnWidthEvent FOnBeforeGetMaxColumnWidth;
	TVTHeaderNotifyEvent FOnColumnChecked;
	TVTColumnCheckChangingEvent FOnColumnChecking;
	TVTColumnClickEvent FOnColumnClick;
	TVTColumnDblClickEvent FOnColumnDblClick;
	TVTHeaderNotifyEvent FOnColumnResize;
	TColumnChangeEvent fOnColumnVisibilityChanged;
	TVTColumnWidthDblClickResizeEvent FOnColumnWidthDblClickResize;
	TVTAfterColumnWidthTrackingEvent FOnAfterColumnWidthTracking;
	TVTBeforeColumnWidthTrackingEvent FOnBeforeColumnWidthTracking;
	TVTColumnWidthTrackingEvent FOnColumnWidthTracking;
	TVTGetHeaderCursorEvent FOnGetHeaderCursor;
	TVTCanSplitterResizeColumnEvent FOnCanSplitterResizeColumn;
	TVTCanSplitterResizeHeaderEvent FOnCanSplitterResizeHeader;
	TVTPaintEvent FOnAfterPaint;
	TVTPaintEvent FOnBeforePaint;
	TVTAfterItemPaintEvent FOnAfterItemPaint;
	TVTBeforeItemPaintEvent FOnBeforeItemPaint;
	TVTBeforeItemEraseEvent FOnBeforeItemErase;
	TVTAfterItemEraseEvent FOnAfterItemErase;
	TVTAfterCellPaintEvent FOnAfterCellPaint;
	TVTBeforeCellPaintEvent FOnBeforeCellPaint;
	TVTHeaderPaintEvent FOnHeaderDraw;
	TVTPrepareButtonImagesEvent FOnPrepareButtonImages;
	TVTHeaderPaintQueryElementsEvent FOnHeaderDrawQueryElements;
	TVTAdvancedHeaderPaintEvent FOnAdvancedHeaderDraw;
	TVTGetLineStyleEvent FOnGetLineStyle;
	TVTBackgroundPaintEvent FOnPaintBackground;
	TVTMeasureItemEvent FOnMeasureItem;
	TVTColumnHeaderSpanningEvent FOnColumnHeaderSpanning;
	TVTGetUserClipboardFormatsEvent FOnGetUserClipboardFormats;
	TVTPaintText FOnPaintText;
	TVTCreateDragManagerEvent FOnCreateDragManager;
	TVTCreateDataObjectEvent FOnCreateDataObject;
	TVTDragAllowedEvent FOnDragAllowed;
	TVTDragOverEvent FOnDragOver;
	TVTDragDropEvent FOnDragDrop;
	TVTHeaderDraggedEvent FOnHeaderDragged;
	TVTHeaderDraggedOutEvent FOnHeaderDraggedOut;
	TVTHeaderDraggingEvent FOnHeaderDragging;
	TVTGetNodeDataSizeEvent FOnGetNodeDataSize;
	TVTBeforeDrawLineImageEvent FOnBeforeDrawLineImage;
	TVTKeyActionEvent FOnKeyAction;
	TVTScrollEvent FOnScroll;
	TVTUpdatingEvent FOnUpdating;
	TVTGetCursorEvent FOnGetCursor;
	TVTStateChangeEvent FOnStateChange;
	TVTGetCellIsEmptyEvent FOnGetCellIsEmpty;
	TVTScrollBarShowEvent FOnShowScrollBar;
	TVTBeforeGetCheckStateEvent FOnBeforeGetCheckState;
	TVTCompareEvent FOnCompareNodes;
	TVTDrawHintEvent FOnDrawHint;
	TVTGetHintSizeEvent FOnGetHintSize;
	TVTHintKindEvent FOnGetHintKind;
	TVTIncrementalSearchEvent FOnIncrementalSearch;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	TVTOperationEvent FOnStartOperation;
	TVTOperationEvent FOnEndOperation;
	bool FVclStyleEnabled;
	int FSelectionCount;
	MESSAGE void __fastcall CMStyleChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMParentDoubleBufferedChange(Winapi::Messages::TMessage &Message);
	void __fastcall AdjustTotalCount(Virtualtrees::Types::PVirtualNode Node, int Value, bool relative = false);
	int __fastcall CalculateCacheEntryCount();
	void __fastcall CalculateVerticalAlignments(Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension &VButtonAlign);
	bool __fastcall ChangeCheckState(Virtualtrees::Types::PVirtualNode Node, TCheckState Value);
	bool __fastcall CollectSelectedNodesLTR(int MainColumn, TDimension NodeLeft, TDimension NodeRight, System::Classes::TAlignment Alignment, const System::Types::TRect &OldRect, const System::Types::TRect &NewRect);
	bool __fastcall CollectSelectedNodesRTL(int MainColumn, TDimension NodeLeft, TDimension NodeRight, System::Classes::TAlignment Alignment, const System::Types::TRect &OldRect, const System::Types::TRect &NewRect);
	void __fastcall ClearNodeBackground(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, bool UseBackground, bool Floating, const System::Types::TRect &R);
	int __fastcall CompareNodePositions(Virtualtrees::Types::PVirtualNode Node1, Virtualtrees::Types::PVirtualNode Node2, bool ConsiderChildrenAbove = false);
	void __fastcall DrawLineImage(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension X, TDimension Y, TDimension H, TDimension VAlign, Virtualtrees::Types::TVTLineType Style, bool Reverse);
	Virtualtrees::Types::PVirtualNode __fastcall FindInPositionCache(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TNodeHeight &CurrentPos)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall FindInPositionCache(TDimension Position, Virtualtrees::Types::TNodeHeight &CurrentPos)/* overload */;
	void __fastcall FixupTotalCount(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall FixupTotalHeight(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetBottomNode();
	TCheckState __fastcall GetCheckState(Virtualtrees::Types::PVirtualNode Node);
	TCheckType __fastcall GetCheckType(Virtualtrees::Types::PVirtualNode Node);
	unsigned __fastcall GetChildCount(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetChildrenInitialized(Virtualtrees::Types::PVirtualNode Node);
	int __fastcall GetCutCopyCount();
	bool __fastcall GetDisabled(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetSyncCheckstateWithSelection(Virtualtrees::Types::PVirtualNode Node);
	_di_IVTDragManager __fastcall GetDragManager();
	bool __fastcall GetExpanded(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetFiltered(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetFullyVisible(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetHasChildren(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetMultiline(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::TNodeHeight __fastcall GetNodeHeight(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetNodeParent(Virtualtrees::Types::PVirtualNode Node);
	System::Types::TPoint __fastcall GetOffsetXY();
	unsigned __fastcall GetRootNodeCount();
	bool __fastcall GetSelected(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetTopNode();
	unsigned __fastcall GetTotalCount();
	System::Byte __fastcall GetVerticalAlignment(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetVisible(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetVisiblePath(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall HandleDrawSelection(TDimension X, TDimension Y);
	void __fastcall HandleCheckboxClick(Virtualtrees::Types::PVirtualNode pHitNode, System::LongInt pKeys);
	bool __fastcall HasVisibleNextSibling(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall HasVisiblePreviousSibling(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall ImageListChange(System::TObject* Sender);
	void __fastcall InitializeFirstColumnValues(Virtualtrees::Types::TVTPaintInfo &PaintInfo);
	void __fastcall InitRootNode(unsigned OldSize = (unsigned)(0x0));
	bool __fastcall IsFirstVisibleChild(Virtualtrees::Types::PVirtualNode Parent, Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall IsLastVisibleChild(Virtualtrees::Types::PVirtualNode Parent, Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall MakeNewNode();
	int __fastcall PackArray(const Virtualtrees::Types::TNodeArray TheArray, int Count);
	void __fastcall FakeReadIdent(System::Classes::TReader* Reader);
	void __fastcall SetAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetAnimationDuration(const unsigned Value);
	void __fastcall SetBackground(Virtualtrees::Types::TVTBackground* const Value);
	void __fastcall SetBackGroundImageTransparent(const bool Value);
	void __fastcall SetBackgroundOffset(const int Index, const TDimension Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetBottomNode(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall SetBottomSpace(const TDimension Value);
	void __fastcall SetButtonFillMode(const Virtualtrees::Types::TVTButtonFillMode Value);
	void __fastcall SetButtonStyle(const Virtualtrees::Types::TVTButtonStyle Value);
	void __fastcall SetCheckImageKind(Virtualtrees::Types::TCheckImageKind Value);
	void __fastcall SetCheckState(Virtualtrees::Types::PVirtualNode Node, TCheckState Value);
	void __fastcall SetCheckType(Virtualtrees::Types::PVirtualNode Node, TCheckType Value);
	void __fastcall SetClipboardFormats(TClipboardFormats* const Value);
	void __fastcall SetColors(TVTColors* const Value);
	void __fastcall SetCustomCheckImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetDefaultNodeHeight(TDimension Value);
	void __fastcall SetDisabled(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetEmptyListMessage(const System::UnicodeString Value);
	void __fastcall SetExpanded(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetFocusedColumn(TColumnIndex Value);
	void __fastcall SetFocusedNode(Virtualtrees::Types::PVirtualNode Value);
	void __fastcall SetFullyVisible(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetHasChildren(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetHeader(Virtualtrees::Header::TVTHeader* const Value);
	void __fastcall SetHotNode(Virtualtrees::Types::PVirtualNode Value);
	void __fastcall SetFiltered(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetIndent(TDimension Value);
	void __fastcall SetLineMode(const Virtualtrees::Types::TVTLineMode Value);
	void __fastcall SetLineStyle(const Virtualtrees::Types::TVTLineStyle Value);
	void __fastcall SetMargin(TDimension Value);
	void __fastcall SetMultiline(Virtualtrees::Types::PVirtualNode Node, const bool Value);
	void __fastcall SetNodeAlignment(const Virtualtrees::Types::TVTNodeAlignment Value);
	void __fastcall SetNodeDataSize(int Value);
	void __fastcall SetNodeHeight(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TNodeHeight Value);
	void __fastcall SetNodeParent(Virtualtrees::Types::PVirtualNode Node, const Virtualtrees::Types::PVirtualNode Value);
	void __fastcall SetOffsetX(const TDimension Value);
	void __fastcall SetOffsetXY(const System::Types::TPoint &Value);
	void __fastcall SetOffsetY(const TDimension Value);
	void __fastcall SetOptions(TCustomVirtualTreeOptions* const Value);
	void __fastcall SetRootNodeCount(unsigned Value);
	void __fastcall SetScrollBarOptions(TScrollBarOptions* Value);
	void __fastcall SetSearchOption(const Virtualtrees::Types::TVTIncrementalSearch Value);
	void __fastcall SetSelected(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetSelectionCurveRadius(const unsigned Value);
	void __fastcall SetStateImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetTextMargin(TDimension Value);
	void __fastcall SetTopNode(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall SetUpdateState(bool Updating);
	void __fastcall SetVerticalAlignment(Virtualtrees::Types::PVirtualNode Node, System::Byte Value);
	HIDESBASE void __fastcall SetVisible(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall SetVisiblePath(Virtualtrees::Types::PVirtualNode Node, bool Value);
	void __fastcall PrepareBackGroundPicture(Virtualtrees::Types::TVTBackground* Source, Vcl::Graphics::TBitmap* DrawingBitmap, TDimension DrawingBitmapWidth, TDimension DrawingBitmapHeight, System::Uitypes::TColor ABkgcolor);
	void __fastcall StaticBackground(Virtualtrees::Types::TVTBackground* Source, TCanvas* Target, const System::Types::TPoint &OffsetPosition, const System::Types::TRect &R, System::Uitypes::TColor aBkgColor);
	void __fastcall TileBackground(Virtualtrees::Types::TVTBackground* Source, TCanvas* Target, const System::Types::TPoint &Offset, const System::Types::TRect &R, System::Uitypes::TColor aBkgColor);
	bool __fastcall ToggleCallback(int Step, int StepSize, void * Data);
	MESSAGE void __fastcall CMColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMBiDiModeChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMBorderChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMDenySubclassing(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDrag(Vcl::Controls::TCMDrag &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Vcl::Controls::TCMHintShow &Message);
	MESSAGE void __fastcall CMHintShowPause(Vcl::Forms::TCMHintShowPause &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Vcl::Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall TVMGetItem(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall TVMGetItemRect(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall TVMGetNextItem(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Winapi::Messages::TWMCancelMode &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMChar &Message);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Winapi::Messages::TWMContextMenu &Message);
	MESSAGE void __fastcall WMCopy(Winapi::Messages::TWMCopy &Message);
	MESSAGE void __fastcall WMCut(Winapi::Messages::TWMCut &Message);
	MESSAGE void __fastcall WMEnable(Winapi::Messages::TWMEnable &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMGetDlgCode &Message);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMHScroll &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKeyDown &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyUp(Winapi::Messages::TWMKeyUp &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMLButtonDblClk &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMLButtonDown &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMLButtonUp &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonDblClk(Winapi::Messages::TWMMButtonDblClk &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonDown(Winapi::Messages::TWMMButtonDown &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonUp(Winapi::Messages::TWMMButtonUp &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCDestroy(Winapi::Messages::TWMNCDestroy &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TWMNCPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TWMPaste &Message);
	MESSAGE void __fastcall WMPrint(Winapi::Messages::TWMPrint &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDblClk(Winapi::Messages::TWMRButtonDblClk &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TWMRButtonDown &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMRButtonUp &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	MESSAGE void __fastcall WMTimer(Winapi::Messages::TWMTimer &Message);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMVScroll &Message);
	TDimension __fastcall GetRangeX();
	HIDESBASE void __fastcall SetDoubleBuffered(const bool Value);
	bool __fastcall GetVclStyleEnabled();
	void __fastcall SetOnPrepareButtonImages(const TVTPrepareButtonImagesEvent Value);
	bool __fastcall IsStored_BackgroundOffsetXY(const int Index);
	bool __fastcall IsStored_BottomSpace();
	bool __fastcall IsStored_DefaultNodeHeight();
	bool __fastcall IsStored_Indent();
	bool __fastcall IsStored_Margin();
	bool __fastcall IsStored_TextMargin();
	
protected:
	bool FFontChanged;
	virtual void __fastcall AutoScale();
	virtual void __fastcall AddToSelection(const Virtualtrees::Types::TNodeArray NewItems, int NewLength, bool ForceInsert = false)/* overload */;
	virtual void __fastcall AdjustPaintCellRect(Virtualtrees::Types::TVTPaintInfo &PaintInfo, TColumnIndex &NextNonEmpty);
	virtual void __fastcall AdjustPanningCursor(TDimension X, TDimension Y);
	void __fastcall AdjustTotalHeight(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TNodeHeight Value, bool relative = false);
	virtual void __fastcall AdviseChangeEvent(bool StructureChange, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TChangeReason Reason);
	virtual unsigned __fastcall AllocateInternalDataArea(unsigned Size);
	virtual void __fastcall Animate(unsigned Steps, unsigned Duration, Virtualtrees::Types::TVTAnimationCallback Callback, void * Data);
	virtual bool __fastcall CalculateSelectionRect(TDimension X, TDimension Y);
	virtual bool __fastcall CanAutoScroll();
	virtual bool __fastcall CanShowDragImage();
	bool __fastcall CanSplitterResizeNode(const System::Types::TPoint &P, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall Change(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall ChangeTreeStatesAsync(const Virtualtrees::Types::TVirtualTreeStates &EnterStates, const Virtualtrees::Types::TVirtualTreeStates &LeaveStates);
	DYNAMIC void __fastcall ChangeScale(int M, int D, bool isDpiChange)/* overload */;
	virtual bool __fastcall CheckParentCheckState(Virtualtrees::Types::PVirtualNode Node, TCheckState NewCheckState);
	void __fastcall ClearDragManager();
	virtual void __fastcall ClearSelection(bool pFireChangeEvent)/* overload */;
	virtual void __fastcall ClearTempCache();
	virtual bool __fastcall ColumnIsEmpty(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	virtual TDimension __fastcall ComputeRTLOffset(bool ExcludeScrollBar = false);
	virtual int __fastcall CountLevelDifference(Virtualtrees::Types::PVirtualNode Node1, Virtualtrees::Types::PVirtualNode Node2);
	virtual unsigned __fastcall CountVisibleChildren(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	void __fastcall DecVisibleCount();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall DeleteNode(Virtualtrees::Types::PVirtualNode Node, bool Reindex, bool ParentClearing)/* overload */;
	virtual Virtualtrees::Types::TDropMode __fastcall DetermineDropMode(const System::Types::TPoint &P, Virtualtrees::Types::THitInfo &HitInfo, System::Types::TRect &NodeRect);
	virtual void __fastcall DetermineHiddenChildrenFlag(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DetermineHiddenChildrenFlagAllNodes();
	virtual void __fastcall DetermineHitPositionLTR(Virtualtrees::Types::THitInfo &HitInfo, TDimension Offset, TDimension Right, System::Classes::TAlignment Alignment);
	virtual void __fastcall DetermineHitPositionRTL(Virtualtrees::Types::THitInfo &HitInfo, TDimension Offset, TDimension Right, System::Classes::TAlignment Alignment);
	virtual int __fastcall DetermineLineImageAndSelectLevel(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TLineImage &LineImage);
	virtual TCheckState __fastcall DetermineNextCheckState(TCheckType CheckType, TCheckState CheckState);
	virtual TScrollDirections __fastcall DetermineScrollDirections(TDimension X, TDimension Y);
	virtual void __fastcall DoAddToSelection(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DoAdvancedHeaderDraw(Virtualtrees::Header::THeaderPaintInfo &PaintInfo, const Virtualtrees::Types::THeaderPaintElements Elements);
	virtual void __fastcall DoAfterCellPaint(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TRect &CellRect);
	virtual void __fastcall DoAfterItemErase(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect);
	virtual void __fastcall DoAfterItemPaint(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect);
	virtual void __fastcall DoAfterPaint(TCanvas* Canvas);
	virtual void __fastcall DoAutoScroll(TDimension X, TDimension Y);
	virtual bool __fastcall DoBeforeDrag(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall DoBeforeCellPaint(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVTCellPaintMode CellPaintMode, const System::Types::TRect &CellRect, System::Types::TRect &ContentRect);
	virtual void __fastcall DoBeforeItemErase(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect, System::Uitypes::TColor &Color, Virtualtrees::Types::TItemEraseAction &EraseAction);
	virtual bool __fastcall DoBeforeItemPaint(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &ItemRect);
	virtual void __fastcall DoBeforePaint(TCanvas* Canvas);
	virtual bool __fastcall DoCancelEdit();
	virtual void __fastcall DoCanEdit(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool &Allowed);
	virtual void __fastcall DoCanSplitterResizeNode(const System::Types::TPoint &P, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool &Allowed);
	virtual void __fastcall DoChange(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DoCheckClick(Virtualtrees::Types::PVirtualNode Node, TCheckState NewCheckState);
	virtual void __fastcall DoChecked(Virtualtrees::Types::PVirtualNode Node);
	virtual bool __fastcall DoChecking(Virtualtrees::Types::PVirtualNode Node, TCheckState &NewCheckState);
	virtual void __fastcall DoCollapsed(Virtualtrees::Types::PVirtualNode Node);
	virtual bool __fastcall DoCollapsing(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DoColumnChecked(TColumnIndex Column);
	virtual bool __fastcall DoColumnChecking(TColumnIndex Column, TCheckState &NewCheckState);
	virtual void __fastcall DoColumnClick(TColumnIndex Column, System::Classes::TShiftState Shift);
	virtual void __fastcall DoColumnDblClick(TColumnIndex Column, System::Classes::TShiftState Shift);
	virtual void __fastcall DoColumnResize(TColumnIndex Column);
	virtual void __fastcall DoColumnVisibilityChanged(const TColumnIndex Column, bool Visible);
	virtual int __fastcall DoCompare(Virtualtrees::Types::PVirtualNode Node1, Virtualtrees::Types::PVirtualNode Node2, TColumnIndex Column);
	virtual Virtualtrees::Types::IDataObject __fastcall DoCreateDataObject();
	virtual _di_IVTDragManager __fastcall DoCreateDragManager();
	virtual _di_IVTEditLink __fastcall DoCreateEditor(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall DoDragging(const System::Types::TPoint &P);
	virtual void __fastcall DoDragExpand();
	virtual void __fastcall DoBeforeDrawLineImage(Virtualtrees::Types::PVirtualNode Node, int Level, TDimension &XPos);
	virtual bool __fastcall DoDragOver(System::TObject* Source, System::Classes::TShiftState Shift, System::Uitypes::TDragState State, const System::Types::TPoint &Pt, Virtualtrees::Types::TDropMode Mode, int &Effect);
	virtual void __fastcall DoDragDrop(System::TObject* Source, const Virtualtrees::Types::TVTDragDataObject DataObject, const TFormatArray Formats, System::Classes::TShiftState Shift, const System::Types::TPoint &Pt, int &Effect, Virtualtrees::Types::TDropMode Mode);
	void __fastcall DoDrawHint(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &R, TColumnIndex Column);
	virtual void __fastcall DoEdit();
	DYNAMIC void __fastcall DoEndDrag(System::TObject* Target, TDimension X, TDimension Y);
	virtual bool __fastcall DoEndEdit(bool pCancel = false);
	virtual void __fastcall DoEndOperation(Virtualtrees::Types::TVTOperationKind OperationKind);
	DYNAMIC void __fastcall DoEnter();
	virtual void __fastcall DoExpanded(Virtualtrees::Types::PVirtualNode Node);
	virtual bool __fastcall DoExpanding(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DoFocusChange(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	virtual bool __fastcall DoFocusChanging(Virtualtrees::Types::PVirtualNode OldNode, Virtualtrees::Types::PVirtualNode NewNode, TColumnIndex OldColumn, TColumnIndex NewColumn);
	virtual void __fastcall DoFocusNode(Virtualtrees::Types::PVirtualNode Node, bool Ask);
	virtual void __fastcall DoFreeNode(Virtualtrees::Types::PVirtualNode Node);
	virtual System::Types::TPoint __fastcall DoGetCellContentMargin(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVTCellContentMarginType CellContentMarginType = (Virtualtrees::Types::TVTCellContentMarginType)(0x0), TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual void __fastcall DoGetCursor(System::Uitypes::TCursor &Cursor);
	virtual void __fastcall DoGetHeaderCursor(Virtualtrees::Types::TVTCursor &Cursor);
	virtual void __fastcall DoGetHintSize(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, System::Types::TRect &R);
	void __fastcall DoGetHintKind(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, TVTHintKind &Kind);
	virtual Vcl::Imglist::TCustomImageList* __fastcall DoGetImageIndex(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind, TColumnIndex Column, bool &Ghosted, TImageIndex &Index);
	virtual void __fastcall DoGetImageText(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind, TColumnIndex Column, System::UnicodeString &ImageText);
	virtual void __fastcall DoGetLineStyle(void * &Bits);
	virtual System::UnicodeString __fastcall DoGetNodeHint(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVTTooltipLineBreakStyle &LineBreakStyle);
	virtual System::UnicodeString __fastcall DoGetNodeTooltip(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVTTooltipLineBreakStyle &LineBreakStyle);
	virtual TDimension __fastcall DoGetNodeExtraWidth(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual TDimension __fastcall DoGetNodeWidth(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual Vcl::Menus::TPopupMenu* __fastcall DoGetPopupMenu(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &Position);
	virtual void __fastcall DoGetUserClipboardFormats(TFormatEtcArray &Formats);
	void __fastcall DoHeaderAddPopupItem(const TColumnIndex Column, Virtualtrees::Types::TAddPopupItemType &Cmd);
	virtual void __fastcall DoHeaderClick(const Virtualtrees::Types::TVTHeaderHitInfo &HitInfo);
	virtual void __fastcall DoHeaderDblClick(const Virtualtrees::Types::TVTHeaderHitInfo &HitInfo);
	virtual void __fastcall DoHeaderDragged(TColumnIndex Column, TColumnPosition OldPosition);
	virtual void __fastcall DoHeaderDraggedOut(TColumnIndex Column, const System::Types::TPoint &DropPosition);
	virtual bool __fastcall DoHeaderDragging(TColumnIndex Column);
	virtual void __fastcall DoHeaderDraw(TCanvas* Canvas, TVirtualTreeColumn* Column, const System::Types::TRect &R, bool Hover, bool Pressed, TVTDropMarkMode DropMark);
	virtual void __fastcall DoHeaderDrawQueryElements(Virtualtrees::Header::THeaderPaintInfo &PaintInfo, Virtualtrees::Types::THeaderPaintElements &Elements);
	virtual void __fastcall DoHeaderMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, TDimension X, TDimension Y);
	virtual void __fastcall DoHeaderMouseMove(System::Classes::TShiftState Shift, TDimension X, TDimension Y);
	virtual void __fastcall DoHeaderMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, TDimension X, TDimension Y);
	virtual void __fastcall DoHotChange(Virtualtrees::Types::PVirtualNode Old, Virtualtrees::Types::PVirtualNode New);
	virtual int __fastcall DoIncrementalSearch(Virtualtrees::Types::PVirtualNode Node, const System::UnicodeString Text);
	virtual bool __fastcall DoInitChildren(Virtualtrees::Types::PVirtualNode Node, unsigned &ChildCount);
	virtual void __fastcall DoInitNode(Virtualtrees::Types::PVirtualNode Parent, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVirtualNodeInitStates &InitStates);
	virtual bool __fastcall DoKeyAction(System::Word &CharCode, System::Classes::TShiftState &Shift);
	virtual void __fastcall DoLoadUserData(Virtualtrees::Types::PVirtualNode Node, System::Classes::TStream* Stream);
	virtual void __fastcall DoMeasureItem(TCanvas* TargetCanvas, Virtualtrees::Types::PVirtualNode Node, TDimension &NodeHeight);
	virtual void __fastcall DoMouseEnter();
	virtual void __fastcall DoMouseLeave();
	virtual void __fastcall DoNodeCopied(Virtualtrees::Types::PVirtualNode Node);
	virtual bool __fastcall DoNodeCopying(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::PVirtualNode NewParent);
	virtual void __fastcall DoNodeClick(const Virtualtrees::Types::THitInfo &HitInfo);
	virtual void __fastcall DoNodeDblClick(const Virtualtrees::Types::THitInfo &HitInfo);
	virtual bool __fastcall DoNodeHeightDblClickResize(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, const System::Types::TPoint &P);
	virtual bool __fastcall DoNodeHeightTracking(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P);
	virtual void __fastcall DoNodeMoved(Virtualtrees::Types::PVirtualNode Node);
	virtual bool __fastcall DoNodeMoving(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::PVirtualNode NewParent);
	virtual bool __fastcall DoPaintBackground(TCanvas* Canvas, const System::Types::TRect &R);
	virtual void __fastcall DoPaintDropMark(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, const System::Types::TRect &R);
	virtual void __fastcall DoPaintNode(Virtualtrees::Types::TVTPaintInfo &PaintInfo);
	virtual void __fastcall DoPaintText(Virtualtrees::Types::PVirtualNode Node, TCanvas* const Canvas, TColumnIndex Column, Virtualtrees::Types::TVSTTextType TextType);
	virtual void __fastcall DoPopupMenu(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &Position);
	virtual void __fastcall DoRemoveFromSelection(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DoReset(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall DoSaveUserData(Virtualtrees::Types::PVirtualNode Node, System::Classes::TStream* Stream);
	virtual void __fastcall DoScroll(TDimension DeltaX, TDimension DeltaY);
	virtual bool __fastcall DoSetOffsetXY(const System::Types::TPoint &Value, Virtualtrees::Types::TScrollUpdateOptions Options, System::Types::PRect ClipRect = (System::Types::PRect)(0x0));
	virtual void __fastcall DoShowScrollBar(int Bar, bool Show);
	DYNAMIC void __fastcall DoStartDrag(Vcl::Controls::TDragObject* &DragObject);
	virtual void __fastcall DoStartOperation(Virtualtrees::Types::TVTOperationKind OperationKind);
	virtual void __fastcall DoStateChange(const Virtualtrees::Types::TVirtualTreeStates &Enter, const Virtualtrees::Types::TVirtualTreeStates &Leave = Virtualtrees::Types::TVirtualTreeStates() );
	virtual void __fastcall DoStructureChange(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TChangeReason Reason);
	virtual void __fastcall DoTimerScroll();
	virtual void __fastcall DoUpdating(Virtualtrees::Types::TVTUpdateState State);
	virtual void __fastcall DoColumnHeaderSpanning(TColumnIndex Column, int &Count);
	virtual bool __fastcall DoValidateCache();
	virtual void __fastcall DragAndDrop(unsigned AllowedEffects, const Virtualtrees::Types::TVTDragDataObject DataObject, int &DragEffect);
	DYNAMIC void __fastcall DragCanceled();
	HIDESBASE virtual HRESULT __fastcall DragDrop(const Virtualtrees::Types::TVTDragDataObject DataObject, int KeyState, const System::Types::TPoint &Pt, int &Effect);
	virtual HRESULT __fastcall DragEnter(int KeyState, const System::Types::TPoint &Pt, int &Effect);
	virtual void __fastcall DragFinished();
	virtual void __fastcall DragLeave();
	HIDESBASE virtual HRESULT __fastcall DragOver(System::TObject* Source, int KeyState, System::Uitypes::TDragState DragState, const System::Types::TPoint &Pt, int &Effect);
	virtual void __fastcall DrawDottedHLine(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension Left, TDimension Right, TDimension Top);
	virtual void __fastcall DrawDottedVLine(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension Top, TDimension Bottom, TDimension Left);
	virtual void __fastcall DrawGridHLine(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension Left, TDimension Right, TDimension Top);
	virtual void __fastcall DrawGridVLine(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension Top, TDimension Bottom, TDimension Left, bool pFixedColumn = false);
	void __fastcall EndOperation(Virtualtrees::Types::TVTOperationKind OperationKind);
	virtual void __fastcall EnsureNodeFocused();
	virtual bool __fastcall FindNodeInSelection(Virtualtrees::Types::PVirtualNode P, int &Index, int LowBound, int HighBound);
	virtual void __fastcall FinishChunkHeader(System::Classes::TStream* Stream, int StartPos, int EndPos);
	HIDESBASE virtual void __fastcall FontChanged(System::TObject* AFont);
	virtual System::Types::TSize __fastcall GetBorderDimensions();
	int __fastcall GetCheckedCount();
	virtual int __fastcall GetCheckImage(Virtualtrees::Types::PVirtualNode Node, TCheckType ImgCheckType = (Virtualtrees::Types::TCheckType)(0x0), TCheckState ImgCheckState = (Virtualtrees::Types::TCheckState)(0x0), bool ImgEnabled = true);
	virtual TVirtualTreeColumnClass __fastcall GetColumnClass();
	virtual TVTHintKind __fastcall GetDefaultHintKind();
	virtual bool __fastcall GetDoubleBuffered();
	virtual Virtualtrees::Header::TVTHeaderClass __fastcall GetHeaderClass();
	virtual Vcl::Controls::THintWindowClass __fastcall GetHintWindowClass() = 0 ;
	virtual void __fastcall GetImageIndex(Virtualtrees::Types::TVTPaintInfo &Info, Virtualtrees::Types::TVTImageKind Kind, Virtualtrees::Types::TVTImageInfoIndex InfoIndex);
	virtual System::Types::TSize __fastcall GetImageSize(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind = (Virtualtrees::Types::TVTImageKind)(0x0), TColumnIndex Column = 0x0, bool IncludePadding = true);
	virtual System::Types::TSize __fastcall GetNodeImageSize _DEPRECATED_ATTRIBUTE1("Use GetImageSize instead") (Virtualtrees::Types::PVirtualNode Node);
	virtual TDimension __fastcall GetMaxRightExtend();
	virtual void __fastcall GetNativeClipboardFormats(TFormatEtcArray &Formats);
	bool __fastcall GetOperationCanceled();
	virtual TTreeOptionsClass __fastcall GetOptionsClass();
	virtual int __fastcall GetSelectedCount();
	virtual void __fastcall HandleHotTrack(TDimension X, TDimension Y);
	virtual void __fastcall HandleIncrementalSearch(System::Word CharCode);
	virtual void __fastcall HandleMouseDblClick(Winapi::Messages::TWMMouse &Message, const Virtualtrees::Types::THitInfo &HitInfo);
	virtual void __fastcall HandleMouseDown(Winapi::Messages::TWMMouse &Message, Virtualtrees::Types::THitInfo &HitInfo);
	virtual void __fastcall HandleMouseUp(Winapi::Messages::TWMMouse &Message, const Virtualtrees::Types::THitInfo &HitInfo);
	void __fastcall HandleClickSelection(Virtualtrees::Types::PVirtualNode LastFocused, Virtualtrees::Types::PVirtualNode NewNode, System::Classes::TShiftState Shift, bool DragPending);
	virtual bool __fastcall HasImage _DEPRECATED_ATTRIBUTE1("Use GetImageSize instead") (Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTImageKind Kind, TColumnIndex Column);
	virtual bool __fastcall HasPopupMenu(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &Pos);
	void __fastcall IncVisibleCount();
	virtual void __fastcall InitChildren(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall InitNode(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall InternalAddFromStream(System::Classes::TStream* Stream, int Version, Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall InternalAddToSelection(Virtualtrees::Types::PVirtualNode Node, bool ForceInsert)/* overload */;
	bool __fastcall InternalAddToSelection(const Virtualtrees::Types::TNodeArray NewItems, int NewLength, bool ForceInsert)/* overload */;
	virtual void __fastcall InternalCacheNode(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall InternalClearSelection();
	virtual void __fastcall InternalConnectNode(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::PVirtualNode Destination, TBaseVirtualTree* Target, Virtualtrees::Types::TVTNodeAttachMode Mode);
	void * __fastcall InternalData(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall InternalDisconnectNode(Virtualtrees::Types::PVirtualNode Node, bool KeepFocus, bool Reindex = true, bool ParentClearing = false);
	void __fastcall InternalSetFocusedColumn(const TColumnIndex index);
	virtual void __fastcall InternalRemoveFromSelection(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall InterruptValidation(bool pWaitForValidationTermination = true);
	void __fastcall InvalidateCache();
	TDimension __fastcall LineWidth();
	virtual void __fastcall Loaded();
	virtual void __fastcall MainColumnChanged();
	virtual void __fastcall MarkCutCopyNodes();
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, TDimension X, TDimension Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall OriginalWMNCPaint(HDC DC);
	virtual void __fastcall Paint();
	virtual void __fastcall PaintCheckImage(TCanvas* Canvas, const Virtualtrees::Types::TVTImageInfo &ImageInfo, bool Selected);
	virtual void __fastcall PaintImage(Virtualtrees::Types::TVTPaintInfo &PaintInfo, Virtualtrees::Types::TVTImageInfoIndex ImageInfoIndex, bool DoOverlay);
	virtual void __fastcall PaintNodeButton(TCanvas* Canvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, const System::Types::TRect &R, TDimension ButtonX, TDimension ButtonY, System::Classes::TBiDiMode BidiMode);
	virtual void __fastcall PaintTreeLines(const Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension IndentSize, const Virtualtrees::Types::TLineImage LineImage);
	virtual void __fastcall PaintSelectionRectangle(TCanvas* Target, TDimension WindowOrgX, const System::Types::TRect &SelectionRect, const System::Types::TRect &TargetRect);
	void __fastcall PrepareBitmaps(bool NeedButtons, bool NeedLines);
	virtual void __fastcall PrepareCell(Virtualtrees::Types::TVTPaintInfo &PaintInfo, TDimension WindowOrgX, TDimension MaxWidth);
	virtual bool __fastcall ReadChunk(System::Classes::TStream* Stream, int Version, Virtualtrees::Types::PVirtualNode Node, int ChunkType, int ChunkSize);
	virtual void __fastcall ReadNode(System::Classes::TStream* Stream, int Version, Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall RedirectFontChangeEvent(TCanvas* Canvas);
	virtual void __fastcall RemoveFromSelection(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall UpdateNextNodeToSelect(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall ResetRangeAnchor();
	virtual void __fastcall RestoreFontChangeEvent(TCanvas* Canvas);
	virtual void __fastcall SelectNodes(Virtualtrees::Types::PVirtualNode StartNode, Virtualtrees::Types::PVirtualNode EndNode, bool AddOnly);
	virtual void __fastcall SetChildCount(Virtualtrees::Types::PVirtualNode Node, unsigned NewChildCount);
	virtual void __fastcall SetFocusedNodeAndColumn(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	void __fastcall SetRangeX(TDimension value);
	virtual void __fastcall SetWindowTheme(const System::UnicodeString Theme);
	void __fastcall SetVisibleCount(unsigned value);
	virtual void __fastcall SkipNode(System::Classes::TStream* Stream);
	void __fastcall StartOperation(Virtualtrees::Types::TVTOperationKind OperationKind);
	virtual void __fastcall StartWheelPanning(const System::Types::TPoint &Position);
	void __fastcall StopTimer(int ID);
	virtual void __fastcall StopWheelPanning();
	virtual void __fastcall StructureChange(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TChangeReason Reason);
	virtual int __fastcall SuggestDropEffect(System::TObject* Source, System::Classes::TShiftState Shift, const System::Types::TPoint &Pt, int AllowedEffects);
	virtual void __fastcall ToggleSelection(Virtualtrees::Types::PVirtualNode StartNode, Virtualtrees::Types::PVirtualNode EndNode);
	void __fastcall TrySetFocus();
	virtual void __fastcall UnselectNodes(Virtualtrees::Types::PVirtualNode StartNode, Virtualtrees::Types::PVirtualNode EndNode);
	void __fastcall UpdateColumnCheckState(TVirtualTreeColumn* Col);
	virtual void __fastcall UpdateDesigner();
	virtual void __fastcall UpdateEditBounds();
	virtual void __fastcall UpdateHeaderRect();
	virtual void __fastcall UpdateStyleElements();
	virtual void __fastcall ValidateCache();
	virtual void __fastcall ValidateNodeDataSize(int &Size);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall WriteChunks(System::Classes::TStream* Stream, Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall WriteNode(System::Classes::TStream* Stream, Virtualtrees::Types::PVirtualNode Node);
	static void __fastcall RaiseVTError(const System::UnicodeString Msg, int HelpContext);
	virtual void __fastcall VclStyleChanged();
	__property bool VclStyleEnabled = {read=GetVclStyleEnabled, nodefault};
	__property unsigned TotalInternalDataSize = {read=FTotalInternalDataSize, nodefault};
	Vcl::Themes::TCustomStyleServices* __fastcall StyleServices(Vcl::Controls::TControl* AControl = (Vcl::Controls::TControl*)(0x0));
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property unsigned AnimationDuration = {read=FAnimationDuration, write=SetAnimationDuration, default=200};
	__property unsigned AutoExpandDelay = {read=FAutoExpandDelay, write=FAutoExpandDelay, default=1000};
	__property unsigned AutoScrollDelay = {read=FAutoScrollDelay, write=FAutoScrollDelay, default=1000};
	__property TAutoScrollInterval AutoScrollInterval = {read=FAutoScrollInterval, write=FAutoScrollInterval, default=1};
	__property Virtualtrees::Types::TVTBackground* Background = {read=FBackground, write=SetBackground};
	__property bool BackGroundImageTransparent = {read=FBackgroundImageTransparent, write=SetBackGroundImageTransparent, default=0};
	__property TDimension BackgroundOffsetX = {read=FBackgroundOffsetX, write=SetBackgroundOffset, stored=IsStored_BackgroundOffsetXY, index=0, nodefault};
	__property TDimension BackgroundOffsetY = {read=FBackgroundOffsetY, write=SetBackgroundOffset, stored=IsStored_BackgroundOffsetXY, index=1, nodefault};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property TDimension BottomSpace = {read=FBottomSpace, write=SetBottomSpace, stored=IsStored_BottomSpace, nodefault};
	__property Virtualtrees::Types::TVTButtonFillMode ButtonFillMode = {read=FButtonFillMode, write=SetButtonFillMode, default=0};
	__property Virtualtrees::Types::TVTButtonStyle ButtonStyle = {read=FButtonStyle, write=SetButtonStyle, default=0};
	__property unsigned ChangeDelay = {read=FChangeDelay, write=FChangeDelay, default=0};
	__property Virtualtrees::Types::TCheckImageKind CheckImageKind = {read=FCheckImageKind, write=SetCheckImageKind, stored=false, default=1};
	__property TClipboardFormats* ClipboardFormats = {read=FClipboardFormats, write=SetClipboardFormats};
	__property TVTColors* Colors = {read=FColors, write=SetColors};
	__property Vcl::Imglist::TCustomImageList* CustomCheckImages = {read=FCustomCheckImages, write=SetCustomCheckImages};
	__property TVTHintKind DefaultHintKind = {read=GetDefaultHintKind, nodefault};
	__property TDimension DefaultNodeHeight = {read=FDefaultNodeHeight, write=SetDefaultNodeHeight, stored=IsStored_DefaultNodeHeight, nodefault};
	__property Virtualtrees::Types::TVTNodeAttachMode DefaultPasteMode = {read=FDefaultPasteMode, write=FDefaultPasteMode, default=4};
	__property int DragHeight = {read=FDragHeight, write=FDragHeight, default=350};
	__property Virtualtrees::Types::TVTDragImageKind DragImageKind = {read=FDragImageKind, write=FDragImageKind, default=0};
	__property Virtualtrees::Types::TDragOperations DragOperations = {read=FDragOperations, write=FDragOperations, default=3};
	__property Virtualtrees::Types::TNodeArray DragSelection = {read=FDragSelection};
	__property int LastDragEffect = {read=FLastDragEffect, nodefault};
	__property Virtualtrees::Types::TVTDragType DragType = {read=FDragType, write=FDragType, default=0};
	__property int DragWidth = {read=FDragWidth, write=FDragWidth, default=200};
	__property Virtualtrees::Types::TVTDrawSelectionMode DrawSelectionMode = {read=FDrawSelectionMode, write=FDrawSelectionMode, default=0};
	__property TColumnIndex EditColumn = {read=FEditColumn, write=FEditColumn, nodefault};
	__property unsigned EditDelay = {read=FEditDelay, write=FEditDelay, default=1000};
	__property TDimension EffectiveOffsetX = {read=FEffectiveOffsetX, nodefault};
	__property System::Types::TRect HeaderRect = {read=FHeaderRect};
	__property Virtualtrees::Types::TVTHintMode HintMode = {read=FHintMode, write=FHintMode, default=0};
	__property TVTHintData HintData = {read=FHintData, write=FHintData};
	__property System::Uitypes::TCursor HotCursor = {read=FHotCursor, write=FHotCursor, default=0};
	__property Vcl::Imglist::TCustomImageList* Images = {read=FImages, write=SetImages};
	__property Virtualtrees::Types::TVTIncrementalSearch IncrementalSearch = {read=FIncrementalSearch, write=SetSearchOption, default=1};
	__property Virtualtrees::Types::TVTSearchDirection IncrementalSearchDirection = {read=FSearchDirection, write=FSearchDirection, default=0};
	__property Virtualtrees::Types::TVTSearchStart IncrementalSearchStart = {read=FSearchStart, write=FSearchStart, default=2};
	__property unsigned IncrementalSearchTimeout = {read=FSearchTimeout, write=FSearchTimeout, default=1000};
	__property TDimension Indent = {read=FIndent, write=SetIndent, stored=IsStored_Indent, nodefault};
	__property System::Types::TPoint LastClickPos = {read=FLastClickPos, write=FLastClickPos};
	__property Virtualtrees::Types::TDropMode LastDropMode = {read=FLastDropMode, write=FLastDropMode, nodefault};
	__property System::Types::TRect LastHintRect = {read=FLastHintRect, write=FLastHintRect};
	__property Virtualtrees::Types::TVTLineMode LineMode = {read=FLineMode, write=SetLineMode, default=0};
	__property Virtualtrees::Types::TVTLineStyle LineStyle = {read=FLineStyle, write=SetLineStyle, default=1};
	__property TDimension Margin = {read=FMargin, write=SetMargin, stored=IsStored_Margin, nodefault};
	__property Virtualtrees::Types::PVirtualNode NextNodeToSelect = {read=FNextNodeToSelect};
	__property Virtualtrees::Types::TVTNodeAlignment NodeAlignment = {read=FNodeAlignment, write=SetNodeAlignment, default=2};
	__property int NodeDataSize = {read=FNodeDataSize, write=SetNodeDataSize, default=-1};
	__property bool OperationCanceled = {read=GetOperationCanceled, nodefault};
	__property Vcl::Graphics::TBitmap* HotMinusBM = {read=FHotMinusBM};
	__property Vcl::Graphics::TBitmap* HotPlusBM = {read=FHotPlusBM};
	__property Vcl::Graphics::TBitmap* MinusBM = {read=FMinusBM};
	__property Vcl::Graphics::TBitmap* PlusBM = {read=FPlusBM};
	__property TDimension RangeX = {read=GetRangeX, nodefault};
	__property Virtualtrees::Types::TNodeHeight RangeY = {read=FRangeY, nodefault};
	__property unsigned RootNodeCount = {read=GetRootNodeCount, write=SetRootNodeCount, default=0};
	__property TScrollBarOptions* ScrollBarOptions = {read=FScrollBarOptions, write=SetScrollBarOptions};
	__property System::Byte SelectionBlendFactor = {read=FSelectionBlendFactor, write=FSelectionBlendFactor, default=128};
	__property unsigned SelectionCurveRadius = {read=FSelectionCurveRadius, write=SetSelectionCurveRadius, default=0};
	__property Vcl::Imglist::TCustomImageList* StateImages = {read=FStateImages, write=SetStateImages};
	__property TDimension TextMargin = {read=FTextMargin, write=SetTextMargin, stored=IsStored_TextMargin, nodefault};
	__property TCustomVirtualTreeOptions* TreeOptions = {read=FOptions, write=SetOptions};
	__property bool WantTabs = {read=FWantTabs, write=FWantTabs, default=0};
	__property bool SyncCheckstateWithSelection[Virtualtrees::Types::PVirtualNode Node] = {read=GetSyncCheckstateWithSelection};
	__property TVTAddToSelectionEvent OnAddToSelection = {read=FOnAddToSelection, write=FOnAddToSelection};
	__property TVTAdvancedHeaderPaintEvent OnAdvancedHeaderDraw = {read=FOnAdvancedHeaderDraw, write=FOnAdvancedHeaderDraw};
	__property TVTAfterAutoFitColumnEvent OnAfterAutoFitColumn = {read=FOnAfterAutoFitColumn, write=FOnAfterAutoFitColumn};
	__property TVTAfterAutoFitColumnsEvent OnAfterAutoFitColumns = {read=FOnAfterAutoFitColumns, write=FOnAfterAutoFitColumns};
	__property TVTAfterCellPaintEvent OnAfterCellPaint = {read=FOnAfterCellPaint, write=FOnAfterCellPaint};
	__property TVTColumnExportEvent OnAfterColumnExport = {read=FOnAfterColumnExport, write=FOnAfterColumnExport};
	__property TVTAfterColumnWidthTrackingEvent OnAfterColumnWidthTracking = {read=FOnAfterColumnWidthTracking, write=FOnAfterColumnWidthTracking};
	__property TVTAfterGetMaxColumnWidthEvent OnAfterGetMaxColumnWidth = {read=FOnAfterGetMaxColumnWidth, write=FOnAfterGetMaxColumnWidth};
	__property TVTTreeExportEvent OnAfterHeaderExport = {read=FOnAfterHeaderExport, write=FOnAfterHeaderExport};
	__property TVTAfterHeaderHeightTrackingEvent OnAfterHeaderHeightTracking = {read=FOnAfterHeaderHeightTracking, write=FOnAfterHeaderHeightTracking};
	__property TVTAfterItemEraseEvent OnAfterItemErase = {read=FOnAfterItemErase, write=FOnAfterItemErase};
	__property TVTAfterItemPaintEvent OnAfterItemPaint = {read=FOnAfterItemPaint, write=FOnAfterItemPaint};
	__property TVTNodeExportEvent OnAfterNodeExport = {read=FOnAfterNodeExport, write=FOnAfterNodeExport};
	__property TVTPaintEvent OnAfterPaint = {read=FOnAfterPaint, write=FOnAfterPaint};
	__property TVTTreeExportEvent OnAfterTreeExport = {read=FOnAfterTreeExport, write=FOnAfterTreeExport};
	__property TVTBeforeAutoFitColumnEvent OnBeforeAutoFitColumn = {read=FOnBeforeAutoFitColumn, write=FOnBeforeAutoFitColumn};
	__property TVTBeforeAutoFitColumnsEvent OnBeforeAutoFitColumns = {read=FOnBeforeAutoFitColumns, write=FOnBeforeAutoFitColumns};
	__property TVTBeforeCellPaintEvent OnBeforeCellPaint = {read=FOnBeforeCellPaint, write=FOnBeforeCellPaint};
	__property TVTColumnExportEvent OnBeforeColumnExport = {read=FOnBeforeColumnExport, write=FOnBeforeColumnExport};
	__property TVTBeforeColumnWidthTrackingEvent OnBeforeColumnWidthTracking = {read=FOnBeforeColumnWidthTracking, write=FOnBeforeColumnWidthTracking};
	__property TVTBeforeDrawLineImageEvent OnBeforeDrawTreeLine = {read=FOnBeforeDrawLineImage, write=FOnBeforeDrawLineImage};
	__property TVTBeforeGetMaxColumnWidthEvent OnBeforeGetMaxColumnWidth = {read=FOnBeforeGetMaxColumnWidth, write=FOnBeforeGetMaxColumnWidth};
	__property TVTTreeExportEvent OnBeforeHeaderExport = {read=FOnBeforeHeaderExport, write=FOnBeforeHeaderExport};
	__property TVTBeforeHeaderHeightTrackingEvent OnBeforeHeaderHeightTracking = {read=FOnBeforeHeaderHeightTracking, write=FOnBeforeHeaderHeightTracking};
	__property TVTBeforeItemEraseEvent OnBeforeItemErase = {read=FOnBeforeItemErase, write=FOnBeforeItemErase};
	__property TVTBeforeItemPaintEvent OnBeforeItemPaint = {read=FOnBeforeItemPaint, write=FOnBeforeItemPaint};
	__property TVTNodeExportEvent OnBeforeNodeExport = {read=FOnBeforeNodeExport, write=FOnBeforeNodeExport};
	__property TVTPaintEvent OnBeforePaint = {read=FOnBeforePaint, write=FOnBeforePaint};
	__property TVTTreeExportEvent OnBeforeTreeExport = {read=FOnBeforeTreeExport, write=FOnBeforeTreeExport};
	__property TVTCanSplitterResizeColumnEvent OnCanSplitterResizeColumn = {read=FOnCanSplitterResizeColumn, write=FOnCanSplitterResizeColumn};
	__property TVTCanSplitterResizeHeaderEvent OnCanSplitterResizeHeader = {read=FOnCanSplitterResizeHeader, write=FOnCanSplitterResizeHeader};
	__property TVTCanSplitterResizeNodeEvent OnCanSplitterResizeNode = {read=FOnCanSplitterResizeNode, write=FOnCanSplitterResizeNode};
	__property TVTChangeEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TVTChangeEvent OnChecked = {read=FOnChecked, write=FOnChecked};
	__property TVTCheckChangingEvent OnChecking = {read=FOnChecking, write=FOnChecking};
	__property TVTChangeEvent OnCollapsed = {read=FOnCollapsed, write=FOnCollapsed};
	__property TVTChangingEvent OnCollapsing = {read=FOnCollapsing, write=FOnCollapsing};
	__property TVTHeaderNotifyEvent OnColumnChecked = {read=FOnColumnChecked, write=FOnColumnChecked};
	__property TVTColumnCheckChangingEvent OnColumnChecking = {read=FOnColumnChecking, write=FOnColumnChecking};
	__property TVTColumnClickEvent OnColumnClick = {read=FOnColumnClick, write=FOnColumnClick};
	__property TVTColumnDblClickEvent OnColumnDblClick = {read=FOnColumnDblClick, write=FOnColumnDblClick};
	__property TVTColumnExportEvent OnColumnExport = {read=FOnColumnExport, write=FOnColumnExport};
	__property TVTHeaderNotifyEvent OnColumnResize = {read=FOnColumnResize, write=FOnColumnResize};
	__property TColumnChangeEvent OnColumnVisibilityChanged = {read=fOnColumnVisibilityChanged, write=fOnColumnVisibilityChanged};
	__property TVTColumnWidthDblClickResizeEvent OnColumnWidthDblClickResize = {read=FOnColumnWidthDblClickResize, write=FOnColumnWidthDblClickResize};
	__property TVTColumnWidthTrackingEvent OnColumnWidthTracking = {read=FOnColumnWidthTracking, write=FOnColumnWidthTracking};
	__property TVTCompareEvent OnCompareNodes = {read=FOnCompareNodes, write=FOnCompareNodes};
	__property TVTCreateDataObjectEvent OnCreateDataObject = {read=FOnCreateDataObject, write=FOnCreateDataObject};
	__property TVTCreateDragManagerEvent OnCreateDragManager = {read=FOnCreateDragManager, write=FOnCreateDragManager};
	__property TVTCreateEditorEvent OnCreateEditor = {read=FOnCreateEditor, write=FOnCreateEditor};
	__property TVTDragAllowedEvent OnDragAllowed = {read=FOnDragAllowed, write=FOnDragAllowed};
	__property TVTDragOverEvent OnDragOver = {read=FOnDragOver, write=FOnDragOver};
	__property TVTDragDropEvent OnDragDrop = {read=FOnDragDrop, write=FOnDragDrop};
	__property TVTDrawHintEvent OnDrawHint = {read=FOnDrawHint, write=FOnDrawHint};
	__property TVTEditCancelEvent OnEditCancelled = {read=FOnEditCancelled, write=FOnEditCancelled};
	__property TVTEditChangingEvent OnEditing = {read=FOnEditing, write=FOnEditing};
	__property TVTEditChangeEvent OnEdited = {read=FOnEdited, write=FOnEdited};
	__property TVTOperationEvent OnEndOperation = {read=FOnEndOperation, write=FOnEndOperation};
	__property TVTChangeEvent OnExpanded = {read=FOnExpanded, write=FOnExpanded};
	__property TVTChangingEvent OnExpanding = {read=FOnExpanding, write=FOnExpanding};
	__property TVTFocusChangeEvent OnFocusChanged = {read=FOnFocusChanged, write=FOnFocusChanged};
	__property TVTFocusChangingEvent OnFocusChanging = {read=FOnFocusChanging, write=FOnFocusChanging};
	__property TVTFreeNodeEvent OnFreeNode = {read=FOnFreeNode, write=FOnFreeNode};
	__property TVTGetCellIsEmptyEvent OnGetCellIsEmpty = {read=FOnGetCellIsEmpty, write=FOnGetCellIsEmpty};
	__property TVTGetCursorEvent OnGetCursor = {read=FOnGetCursor, write=FOnGetCursor};
	__property TVTGetHeaderCursorEvent OnGetHeaderCursor = {read=FOnGetHeaderCursor, write=FOnGetHeaderCursor};
	__property TVTHelpContextEvent OnGetHelpContext = {read=FOnGetHelpContext, write=FOnGetHelpContext};
	__property TVTGetHintSizeEvent OnGetHintSize = {read=FOnGetHintSize, write=FOnGetHintSize};
	__property TVTHintKindEvent OnGetHintKind = {read=FOnGetHintKind, write=FOnGetHintKind};
	__property TVTGetImageEvent OnGetImageIndex = {read=FOnGetImage, write=FOnGetImage};
	__property TVTGetImageExEvent OnGetImageIndexEx = {read=FOnGetImageEx, write=FOnGetImageEx};
	__property TVTGetImageTextEvent OnGetImageText = {read=FOnGetImageText, write=FOnGetImageText};
	__property TVTGetLineStyleEvent OnGetLineStyle = {read=FOnGetLineStyle, write=FOnGetLineStyle};
	__property TVTGetNodeDataSizeEvent OnGetNodeDataSize = {read=FOnGetNodeDataSize, write=FOnGetNodeDataSize};
	__property TVTPopupEvent OnGetPopupMenu = {read=FOnGetPopupMenu, write=FOnGetPopupMenu};
	__property TVTGetUserClipboardFormatsEvent OnGetUserClipboardFormats = {read=FOnGetUserClipboardFormats, write=FOnGetUserClipboardFormats};
	__property TVTHeaderAddPopupItemEvent OnHeaderAddPopupItem = {read=FOnHeaderAddPopupItem, write=FOnHeaderAddPopupItem};
	__property TVTHeaderClickEvent OnHeaderClick = {read=FOnHeaderClick, write=FOnHeaderClick};
	__property TVTHeaderClickEvent OnHeaderDblClick = {read=FOnHeaderDblClick, write=FOnHeaderDblClick};
	__property TVTHeaderDraggedEvent OnHeaderDragged = {read=FOnHeaderDragged, write=FOnHeaderDragged};
	__property TVTHeaderDraggedOutEvent OnHeaderDraggedOut = {read=FOnHeaderDraggedOut, write=FOnHeaderDraggedOut};
	__property TVTHeaderDraggingEvent OnHeaderDragging = {read=FOnHeaderDragging, write=FOnHeaderDragging};
	__property TVTHeaderPaintEvent OnHeaderDraw = {read=FOnHeaderDraw, write=FOnHeaderDraw};
	__property TVTHeaderPaintQueryElementsEvent OnHeaderDrawQueryElements = {read=FOnHeaderDrawQueryElements, write=FOnHeaderDrawQueryElements};
	__property TVTHeaderHeightTrackingEvent OnHeaderHeightTracking = {read=FOnHeaderHeightTracking, write=FOnHeaderHeightTracking};
	__property TVTHeaderHeightDblClickResizeEvent OnHeaderHeightDblClickResize = {read=FOnHeaderHeightDblClickResize, write=FOnHeaderHeightDblClickResize};
	__property TVTHeaderMouseEvent OnHeaderMouseDown = {read=FOnHeaderMouseDown, write=FOnHeaderMouseDown};
	__property TVTHeaderMouseMoveEvent OnHeaderMouseMove = {read=FOnHeaderMouseMove, write=FOnHeaderMouseMove};
	__property TVTHeaderMouseEvent OnHeaderMouseUp = {read=FOnHeaderMouseUp, write=FOnHeaderMouseUp};
	__property TVTHotNodeChangeEvent OnHotChange = {read=FOnHotChange, write=FOnHotChange};
	__property TVTIncrementalSearchEvent OnIncrementalSearch = {read=FOnIncrementalSearch, write=FOnIncrementalSearch};
	__property TVTInitChildrenEvent OnInitChildren = {read=FOnInitChildren, write=FOnInitChildren};
	__property TVTInitNodeEvent OnInitNode = {read=FOnInitNode, write=FOnInitNode};
	__property TVTKeyActionEvent OnKeyAction = {read=FOnKeyAction, write=FOnKeyAction};
	__property TVTSaveNodeEvent OnLoadNode = {read=FOnLoadNode, write=FOnLoadNode};
	__property TVTSaveTreeEvent OnLoadTree = {read=FOnLoadTree, write=FOnLoadTree};
	__property TVTMeasureItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property TVTNodeClickEvent OnNodeClick = {read=FOnNodeClick, write=FOnNodeClick};
	__property TVTNodeCopiedEvent OnNodeCopied = {read=FOnNodeCopied, write=FOnNodeCopied};
	__property TVTNodeCopyingEvent OnNodeCopying = {read=FOnNodeCopying, write=FOnNodeCopying};
	__property TVTNodeClickEvent OnNodeDblClick = {read=FOnNodeDblClick, write=FOnNodeDblClick};
	__property TVTNodeExportEvent OnNodeExport = {read=FOnNodeExport, write=FOnNodeExport};
	__property TVTNodeHeightTrackingEvent OnNodeHeightTracking = {read=FOnNodeHeightTracking, write=FOnNodeHeightTracking};
	__property TVTNodeHeightDblClickResizeEvent OnNodeHeightDblClickResize = {read=FOnNodeHeightDblClickResize, write=FOnNodeHeightDblClickResize};
	__property TVTNodeMovedEvent OnNodeMoved = {read=FOnNodeMoved, write=FOnNodeMoved};
	__property TVTNodeMovingEvent OnNodeMoving = {read=FOnNodeMoving, write=FOnNodeMoving};
	__property TVTBackgroundPaintEvent OnPaintBackground = {read=FOnPaintBackground, write=FOnPaintBackground};
	__property TVTPaintText OnPaintText = {read=FOnPaintText, write=FOnPaintText};
	__property TVTPrepareButtonImagesEvent OnPrepareButtonBitmaps = {read=FOnPrepareButtonImages, write=SetOnPrepareButtonImages};
	__property TVTRemoveFromSelectionEvent OnRemoveFromSelection = {read=FOnRemoveFromSelection, write=FOnRemoveFromSelection};
	__property TVTChangeEvent OnResetNode = {read=FOnResetNode, write=FOnResetNode};
	__property TVTSaveNodeEvent OnSaveNode = {read=FOnSaveNode, write=FOnSaveNode};
	__property TVTSaveTreeEvent OnSaveTree = {read=FOnSaveTree, write=FOnSaveTree};
	__property TVTScrollEvent OnScroll = {read=FOnScroll, write=FOnScroll};
	__property TVTScrollBarShowEvent OnShowScrollBar = {read=FOnShowScrollBar, write=FOnShowScrollBar};
	__property TVTBeforeGetCheckStateEvent OnBeforeGetCheckState = {read=FOnBeforeGetCheckState, write=FOnBeforeGetCheckState};
	__property TVTOperationEvent OnStartOperation = {read=FOnStartOperation, write=FOnStartOperation};
	__property TVTStateChangeEvent OnStateChange = {read=FOnStateChange, write=FOnStateChange};
	__property TVTStructureChangeEvent OnStructureChange = {read=FOnStructureChange, write=FOnStructureChange};
	__property TVTUpdatingEvent OnUpdating = {read=FOnUpdating, write=FOnUpdating};
	__property TVTColumnHeaderSpanningEvent OnColumnHeaderSpanning = {read=FOnColumnHeaderSpanning, write=FOnColumnHeaderSpanning};
	
public:
	__fastcall virtual TBaseVirtualTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TBaseVirtualTree();
	unsigned __fastcall AbsoluteIndex(Virtualtrees::Types::PVirtualNode Node);
	virtual Virtualtrees::Types::PVirtualNode __fastcall AddChild(Virtualtrees::Types::PVirtualNode Parent, void * UserData = (void *)(0x0))/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall AddChild(Virtualtrees::Types::PVirtualNode Parent, const System::_di_IInterface UserData)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall AddChild(Virtualtrees::Types::PVirtualNode Parent, System::TObject* const UserData)/* overload */;
	void __fastcall AddFromStream(System::Classes::TStream* Stream, Virtualtrees::Types::PVirtualNode TargetNode);
	virtual void __fastcall AddToSelection(Virtualtrees::Types::PVirtualNode Node, bool NotifySynced)/* overload */;
	virtual void __fastcall AfterConstruction();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE void __fastcall BeginDrag(bool Immediate, int Threshold = 0xffffffff);
	void __fastcall BeginSynch();
	virtual void __fastcall BeginUpdate();
	void __fastcall CancelCutOrCopy();
	bool __fastcall CancelEditNode();
	void __fastcall CancelOperation();
	virtual bool __fastcall CanEdit(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	DYNAMIC bool __fastcall CanFocus();
	virtual void __fastcall Clear();
	void __fastcall ClearChecked();
	void __fastcall ClearSelection()/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall CopyTo(Virtualtrees::Types::PVirtualNode Source, TBaseVirtualTree* Tree, Virtualtrees::Types::TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall CopyTo(Virtualtrees::Types::PVirtualNode Source, Virtualtrees::Types::PVirtualNode Target, Virtualtrees::Types::TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	virtual void __fastcall CutToClipboard();
	void __fastcall DeleteChildren(Virtualtrees::Types::PVirtualNode Node, bool ResetHasChildren = false);
	void __fastcall DeleteNode(Virtualtrees::Types::PVirtualNode Node, bool pReIndex = true)/* overload */;
	void __fastcall DeleteNodes(const Virtualtrees::Types::TNodeArray pNodes);
	virtual void __fastcall DeleteSelectedNodes();
	HIDESBASE bool __fastcall Dragging();
	virtual void __fastcall DrawGridLine(TCanvas* Canvas, const System::Types::TRect &R);
	virtual bool __fastcall EditNode(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column);
	bool __fastcall EndEditNode();
	void __fastcall EndSynch();
	virtual void __fastcall EndUpdate();
	virtual void __fastcall EnsureNodeSelected(bool pAfterDeletion);
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	void __fastcall FinishCutOrCopy();
	void __fastcall FlushClipboard();
	virtual void __fastcall FullCollapse(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0));
	virtual void __fastcall FullExpand(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0));
	DYNAMIC System::Classes::TAlignment __fastcall GetControlsAlignment();
	System::Types::TRect __fastcall GetDisplayRect(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, bool TextOnly, bool Unclipped = false, bool ApplyCellContentMargin = false);
	bool __fastcall GetEffectivelyFiltered(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall GetEffectivelyVisible(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirst(bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstChecked(TCheckState State = (Virtualtrees::Types::TCheckState)(0x2), bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstChild(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstChildNoInit(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstCutCopy(bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstInitialized(bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstLeaf();
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstLevel(unsigned NodeLevel);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstNoInit(bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstSelected(bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstVisible(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstVisibleChild(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstVisibleChildNoInit(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetFirstVisibleNoInit(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	virtual void __fastcall GetHitTestInfoAt(TDimension X, TDimension Y, bool Relative, Virtualtrees::Types::THitInfo &HitInfo, System::Classes::TShiftState ShiftState = System::Classes::TShiftState() );
	Virtualtrees::Types::PVirtualNode __fastcall GetLast(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastInitialized(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastNoInit(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastChild(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastChildNoInit(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastSelected(bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastVisible(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastVisibleChild(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastVisibleChildNoInit(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetLastVisibleNoInit(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	virtual TDimension __fastcall GetMaxColumnWidth(TColumnIndex Column, bool UseSmartColumnWidth = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNext(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextChecked(Virtualtrees::Types::PVirtualNode Node, TCheckState State = (Virtualtrees::Types::TCheckState)(0x2), bool ConsiderChildrenAbove = false)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall GetNextChecked(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall GetNextCutCopy(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextInitialized(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextLeaf(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextLevel(Virtualtrees::Types::PVirtualNode Node, unsigned NodeLevel);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextNoInit(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextSelected(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextSibling(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextSiblingNoInit(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextVisible(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = true);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextVisibleNoInit(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = true);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextVisibleSibling(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNextVisibleSiblingNoInit(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetNodeAt(const System::Types::TPoint &P)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall GetNodeAt(TDimension X, TDimension Y)/* overload */;
	Virtualtrees::Types::PVirtualNode __fastcall GetNodeAt(TDimension X, TDimension Y, bool Relative, TDimension &NodeTop)/* overload */;
	void * __fastcall GetNodeData(Virtualtrees::Types::PVirtualNode Node)/* overload */;
	template<typename T> T __fastcall GetNodeData(Virtualtrees::Types::PVirtualNode pNode)/* overload */;
	template<typename T> System::DynamicArray<T> __fastcall GetSelectedData()/* overload */;
	template<typename T> T __fastcall GetInterfaceFromNodeData(Virtualtrees::Types::PVirtualNode pNode)/* overload */;
	template<typename T> T __fastcall GetNodeDataAt(int pXCoord, int pYCoord);
	template<typename T> T __fastcall GetFirstSelectedNodeData();
	unsigned __fastcall GetNodeLevel(Virtualtrees::Types::PVirtualNode Node);
	int __fastcall GetNodeLevelForSelectConstraint(Virtualtrees::Types::PVirtualNode Node);
	TDimension __fastcall GetOffset(Virtualtrees::Types::TVTElement pElement, Virtualtrees::Types::PVirtualNode pNode);
	void __fastcall GetOffsets(Virtualtrees::Types::PVirtualNode pNode, /* out */ Virtualtrees::Types::TVTOffsets &pOffsets, Virtualtrees::Types::TVTElement pElement = (Virtualtrees::Types::TVTElement)(0x8), int pColumn = 0xffffffff);
	Virtualtrees::Types::PVirtualNode __fastcall GetPrevious(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousChecked(Virtualtrees::Types::PVirtualNode Node, TCheckState State = (Virtualtrees::Types::TCheckState)(0x2), bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousCutCopy(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousInitialized(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousLeaf(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousLevel(Virtualtrees::Types::PVirtualNode Node, unsigned NodeLevel);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousNoInit(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousSelected(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousSibling(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousSiblingNoInit(Virtualtrees::Types::PVirtualNode Node);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousVisible(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = true);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousVisibleNoInit(Virtualtrees::Types::PVirtualNode Node, bool ConsiderChildrenAbove = true);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousVisibleSibling(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetPreviousVisibleSiblingNoInit(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	virtual Virtualtrees::Types::TNodeArray __fastcall GetSortedCutCopySet(bool Resolve);
	virtual Virtualtrees::Types::TNodeArray __fastcall GetSortedSelection(bool Resolve);
	virtual void __fastcall GetTextInfo(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TFont* const AFont, System::Types::TRect &R, System::UnicodeString &Text);
	System::Types::TRect __fastcall GetTreeRect();
	Virtualtrees::Types::PVirtualNode __fastcall GetVisibleParent(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	Virtualtrees::Types::PVirtualNode __fastcall GetTopInvisibleParent(Virtualtrees::Types::PVirtualNode Node);
	bool __fastcall HasAsParent(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::PVirtualNode PotentialParent);
	Virtualtrees::Types::PVirtualNode __fastcall InsertNode(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Types::TVTNodeAttachMode Mode, void * UserData = (void *)(0x0));
	void __fastcall InvalidateChildren(Virtualtrees::Types::PVirtualNode Node, bool Recursive);
	void __fastcall InvalidateColumn(TColumnIndex Column);
	virtual System::Types::TRect __fastcall InvalidateNode(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall InvalidateToBottom(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall InvertSelection(bool VisibleOnly);
	bool __fastcall IsEditing();
	bool __fastcall IsMouseSelecting();
	bool __fastcall IsEmpty();
	bool __fastcall IsUpdating();
	Virtualtrees::Types::PVirtualNode __fastcall IterateSubtree(Virtualtrees::Types::PVirtualNode StartNode, _di_TVTGetNodeProc Callback, void * Data, Virtualtrees::Types::TVirtualNodeStates Filter = Virtualtrees::Types::TVirtualNodeStates() , bool DoInit = false, bool ChildNodesOnly = false);
	virtual void __fastcall LoadFromFile(const System::Sysutils::TFileName FileName);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall MeasureItemHeight(TCanvas* const Canvas, Virtualtrees::Types::PVirtualNode Node);
	void __fastcall MoveTo(Virtualtrees::Types::PVirtualNode Source, Virtualtrees::Types::PVirtualNode Target, Virtualtrees::Types::TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	void __fastcall MoveTo(Virtualtrees::Types::PVirtualNode Node, TBaseVirtualTree* Tree, Virtualtrees::Types::TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	virtual void __fastcall PaintTree(TCanvas* TargetCanvas, const System::Types::TRect &Window, const System::Types::TPoint &Target, Virtualtrees::Types::TVTInternalPaintOptions PaintOptions, Vcl::Graphics::TPixelFormat PixelFormat = (Vcl::Graphics::TPixelFormat)(0x0));
	void __fastcall PrepareDragImage(const System::Types::TPoint &HotSpot, const Virtualtrees::Types::TVTDragDataObject DataObject);
	void __fastcall Print(Vcl::Printers::TPrinter* Printer, bool PrintHeader);
	bool __fastcall ProcessDrop(const Virtualtrees::Types::TVTDragDataObject DataObject, Virtualtrees::Types::PVirtualNode TargetNode, int &Effect, Virtualtrees::Types::TVTNodeAttachMode Mode);
	bool __fastcall ProcessOLEData(TBaseVirtualTree* Source, const Virtualtrees::Types::IDataObject DataObject, Virtualtrees::Types::PVirtualNode TargetNode, Virtualtrees::Types::TVTNodeAttachMode Mode, bool Optimized);
	void __fastcall RepaintNode(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall ReinitChildren(Virtualtrees::Types::PVirtualNode Node, bool Recursive, bool ForceReinit = false);
	void __fastcall InitRecursive(Virtualtrees::Types::PVirtualNode Node, unsigned Levels = (unsigned)(0x7fffffff), bool pVisibleOnly = true);
	virtual void __fastcall ReinitNode(Virtualtrees::Types::PVirtualNode Node, bool Recursive, bool ForceReinit = false);
	virtual void __fastcall ResetNode(Virtualtrees::Types::PVirtualNode Node);
	void __fastcall SaveToFile(const System::Sysutils::TFileName FileName);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream, Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0));
	TDimension __fastcall ScaledPixels(TDimension pPixels);
	void __fastcall ScaleNodeHeights(TDimension M, TDimension D);
	bool __fastcall ScrollIntoView(Virtualtrees::Types::PVirtualNode Node, bool Center, bool Horizontally = false)/* overload */;
	bool __fastcall ScrollIntoView(TColumnIndex Column, bool Center, Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0))/* overload */;
	void __fastcall SelectAll(bool VisibleOnly);
	void __fastcall SetCheckStateForAll(TCheckState aCheckState, bool pSelectedOnly, bool pExcludeDisabled = true);
	void __fastcall SetNodeData(Virtualtrees::Types::PVirtualNode pNode, void * pUserData)/* overload */;
	void __fastcall SetNodeData(Virtualtrees::Types::PVirtualNode pNode, const System::_di_IInterface pUserData)/* overload */;
	template<typename T> void __fastcall SetNodeData(Virtualtrees::Types::PVirtualNode pNode, T pUserData)/* overload */;
	virtual void __fastcall Sort(Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, TSortDirection Direction, bool DoInit = true);
	virtual void __fastcall SortTree(TColumnIndex Column, TSortDirection Direction, bool DoInit = true);
	void __fastcall ToggleNode(Virtualtrees::Types::PVirtualNode Node);
	virtual void __fastcall UpdateHorizontalRange();
	void __fastcall UpdateHorizontalScrollBar(bool DoRepaint);
	void __fastcall UpdateRanges();
	virtual void __fastcall UpdateScrollBars(bool DoRepaint);
	void __fastcall UpdateVerticalRange();
	void __fastcall UpdateVerticalScrollBar(bool DoRepaint);
	HIDESBASE bool __fastcall UseRightToLeftReading();
	void __fastcall ValidateChildren(Virtualtrees::Types::PVirtualNode Node, bool Recursive);
	void __fastcall ValidateNode(Virtualtrees::Types::PVirtualNode Node, bool Recursive);
	TVTVirtualNodeEnumeration __fastcall Nodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall CheckedNodes(TCheckState State = (Virtualtrees::Types::TCheckState)(0x2), bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall ChildNodes(Virtualtrees::Types::PVirtualNode Node);
	TVTVirtualNodeEnumeration __fastcall CutCopyNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall InitializedNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall LeafNodes();
	TVTVirtualNodeEnumeration __fastcall LevelNodes(unsigned NodeLevel);
	TVTVirtualNodeEnumeration __fastcall NoInitNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall SelectedNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall VisibleNodes(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	TVTVirtualNodeEnumeration __fastcall VisibleChildNodes(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	TVTVirtualNodeEnumeration __fastcall VisibleChildNoInitNodes(Virtualtrees::Types::PVirtualNode Node, bool IncludeFiltered = false);
	TVTVirtualNodeEnumeration __fastcall VisibleNoInitNodes(Virtualtrees::Types::PVirtualNode Node = (Virtualtrees::Types::PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	__property Virtualtrees::Types::PVirtualNode BottomNode = {read=GetBottomNode, write=SetBottomNode};
	__property int CheckedCount = {read=GetCheckedCount, nodefault};
	__property Vcl::Imglist::TCustomImageList* CheckImages = {read=FCheckImages};
	__property TCheckState CheckState[Virtualtrees::Types::PVirtualNode Node] = {read=GetCheckState, write=SetCheckState};
	__property TCheckType CheckType[Virtualtrees::Types::PVirtualNode Node] = {read=GetCheckType, write=SetCheckType};
	__property unsigned ChildCount[Virtualtrees::Types::PVirtualNode Node] = {read=GetChildCount, write=SetChildCount};
	__property bool ChildrenInitialized[Virtualtrees::Types::PVirtualNode Node] = {read=GetChildrenInitialized};
	__property int CutCopyCount = {read=GetCutCopyCount, nodefault};
	__property _di_IVTDragManager DragManager = {read=GetDragManager};
	__property Virtualtrees::Types::PVirtualNode DropTargetNode = {read=FDropTargetNode, write=FDropTargetNode};
	__property _di_IVTEditLink EditLink = {read=FEditLink};
	__property System::UnicodeString EmptyListMessage = {read=FEmptyListMessage, write=SetEmptyListMessage};
	__property bool Expanded[Virtualtrees::Types::PVirtualNode Node] = {read=GetExpanded, write=SetExpanded};
	__property TColumnIndex FocusedColumn = {read=FFocusedColumn, write=SetFocusedColumn, default=-2};
	__property Virtualtrees::Types::PVirtualNode FocusedNode = {read=FFocusedNode, write=SetFocusedNode};
	__property Font;
	__property bool FullyVisible[Virtualtrees::Types::PVirtualNode Node] = {read=GetFullyVisible, write=SetFullyVisible};
	__property bool HasChildren[Virtualtrees::Types::PVirtualNode Node] = {read=GetHasChildren, write=SetHasChildren};
	__property Virtualtrees::Header::TVTHeader* Header = {read=FHeader, write=SetHeader};
	__property Virtualtrees::Types::PVirtualNode HotNode = {read=FCurrentHotNode, write=SetHotNode};
	__property TColumnIndex HotColumn = {read=FCurrentHotColumn, nodefault};
	__property bool IsDisabled[Virtualtrees::Types::PVirtualNode Node] = {read=GetDisabled, write=SetDisabled};
	__property bool IsEffectivelyFiltered[Virtualtrees::Types::PVirtualNode Node] = {read=GetEffectivelyFiltered};
	__property bool IsEffectivelyVisible[Virtualtrees::Types::PVirtualNode Node] = {read=GetEffectivelyVisible};
	__property bool IsFiltered[Virtualtrees::Types::PVirtualNode Node] = {read=GetFiltered, write=SetFiltered};
	__property bool IsVisible[Virtualtrees::Types::PVirtualNode Node] = {read=GetVisible, write=SetVisible};
	__property bool MultiLine[Virtualtrees::Types::PVirtualNode Node] = {read=GetMultiline, write=SetMultiline};
	__property Virtualtrees::Types::TNodeHeight NodeHeight[Virtualtrees::Types::PVirtualNode Node] = {read=GetNodeHeight, write=SetNodeHeight};
	__property Virtualtrees::Types::PVirtualNode NodeParent[Virtualtrees::Types::PVirtualNode Node] = {read=GetNodeParent, write=SetNodeParent};
	__property TDimension OffsetX = {read=FOffsetX, write=SetOffsetX, nodefault};
	__property System::Types::TPoint OffsetXY = {read=GetOffsetXY, write=SetOffsetXY};
	__property TDimension OffsetY = {read=FOffsetY, write=SetOffsetY, nodefault};
	__property unsigned OperationCount = {read=FOperationCount, nodefault};
	__property Virtualtrees::Types::PVirtualNode RootNode = {read=FRoot};
	__property System::UnicodeString SearchBuffer = {read=FSearchBuffer};
	__property bool Selected[Virtualtrees::Types::PVirtualNode Node] = {read=GetSelected, write=SetSelected};
	__property bool SelectionLocked = {read=FSelectionLocked, write=FSelectionLocked, nodefault};
	__property unsigned TotalCount = {read=GetTotalCount, nodefault};
	__property Virtualtrees::Types::TVirtualTreeStates TreeStates = {read=FStates, write=FStates};
	__property int SelectedCount = {read=FSelectionCount, nodefault};
	__property Virtualtrees::Types::PVirtualNode TopNode = {read=GetTopNode, write=SetTopNode};
	__property System::Byte VerticalAlignment[Virtualtrees::Types::PVirtualNode Node] = {read=GetVerticalAlignment, write=SetVerticalAlignment};
	__property unsigned VisibleCount = {read=FVisibleCount, nodefault};
	__property bool VisiblePath[Virtualtrees::Types::PVirtualNode Node] = {read=GetVisiblePath, write=SetVisiblePath};
	__property unsigned UpdateCount = {read=FUpdateCount, nodefault};
	__property bool DoubleBuffered = {read=GetDoubleBuffered, write=SetDoubleBuffered, default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TBaseVirtualTree(HWND ParentWindow) : Virtualtrees::Baseancestorvcl::TVTBaseAncestorVcl(ParentWindow) { }
	
	/* Hoisted overloads: */
	
protected:
	DYNAMIC inline void __fastcall  ChangeScale(int M, int D){ Vcl::Controls::TControl::ChangeScale(M, D); }
	
};


typedef void __fastcall (__closure *TVTDrawNodeEvent)(TBaseVirtualTree* Sender, const Virtualtrees::Types::TVTPaintInfo &PaintInfo);

typedef void __fastcall (__closure *TVTGetCellContentMarginEvent)(TBaseVirtualTree* Sender, TCanvas* HintCanvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, Virtualtrees::Types::TVTCellContentMarginType CellContentMarginType, System::Types::TPoint &CellContentMargin);

typedef void __fastcall (__closure *TVTGetNodeWidthEvent)(TBaseVirtualTree* Sender, TCanvas* HintCanvas, Virtualtrees::Types::PVirtualNode Node, TColumnIndex Column, TDimension &NodeWidth);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TBaseVirtualTree* __fastcall TreeFromNode(Virtualtrees::Types::PVirtualNode Node);
}	/* namespace Basetree */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_BASETREE)
using namespace Virtualtrees::Basetree;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_BasetreeHPP
