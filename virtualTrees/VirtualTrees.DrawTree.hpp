// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.DrawTree.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_DrawtreeHPP
#define Virtualtrees_DrawtreeHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <Vcl.Themes.hpp>
#include <VirtualTrees.Types.hpp>
#include <VirtualTrees.BaseTree.hpp>
#include <VirtualTrees.AncestorVCL.hpp>
#include <VirtualTrees.BaseAncestorVCL.hpp>
#include <Vcl.Controls.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <VirtualTrees.Colors.hpp>
#include <Vcl.ImgList.hpp>
#include <VirtualTrees.Header.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Drawtree
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomVirtualDrawTree;
class DELPHICLASS TVirtualDrawTree;
//-- type declarations -------------------------------------------------------
typedef Virtualtrees::Ancestorvcl::TVTAncestorVcl TVTAncestor;

class PASCALIMPLEMENTATION TCustomVirtualDrawTree : public Virtualtrees::Ancestorvcl::TVTAncestorVcl
{
	typedef Virtualtrees::Ancestorvcl::TVTAncestorVcl inherited;
	
private:
	Virtualtrees::Basetree::TVTDrawNodeEvent FOnDrawNode;
	Virtualtrees::Basetree::TVTGetCellContentMarginEvent FOnGetCellContentMargin;
	Virtualtrees::Basetree::TVTGetNodeWidthEvent FOnGetNodeWidth;
	
protected:
	virtual System::Types::TPoint __fastcall DoGetCellContentMargin(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Basetree::TColumnIndex Column, Virtualtrees::Types::TVTCellContentMarginType CellContentMarginType = (Virtualtrees::Types::TVTCellContentMarginType)(0x0), Virtualtrees::Basetree::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual Virtualtrees::Basetree::TDimension __fastcall DoGetNodeWidth(Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Basetree::TColumnIndex Column, Virtualtrees::Basetree::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual void __fastcall DoPaintNode(Virtualtrees::Types::TVTPaintInfo &PaintInfo);
	virtual Virtualtrees::Basetree::TVTHintKind __fastcall GetDefaultHintKind();
	__property Virtualtrees::Basetree::TVTDrawNodeEvent OnDrawNode = {read=FOnDrawNode, write=FOnDrawNode};
	__property Virtualtrees::Basetree::TVTGetCellContentMarginEvent OnGetCellContentMargin = {read=FOnGetCellContentMargin, write=FOnGetCellContentMargin};
	__property Virtualtrees::Basetree::TVTGetNodeWidthEvent OnGetNodeWidth = {read=FOnGetNodeWidth, write=FOnGetNodeWidth};
public:
	/* TBaseVirtualTree.Create */ inline __fastcall virtual TCustomVirtualDrawTree(System::Classes::TComponent* AOwner) : Virtualtrees::Ancestorvcl::TVTAncestorVcl(AOwner) { }
	/* TBaseVirtualTree.Destroy */ inline __fastcall virtual ~TCustomVirtualDrawTree() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomVirtualDrawTree(HWND ParentWindow) : Virtualtrees::Ancestorvcl::TVTAncestorVcl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TVirtualDrawTree : public TCustomVirtualDrawTree
{
	typedef TCustomVirtualDrawTree inherited;
	
private:
	Virtualtrees::Basetree::TVirtualTreeOptions* __fastcall GetOptions();
	HIDESBASE void __fastcall SetOptions(Virtualtrees::Basetree::TVirtualTreeOptions* const Value);
	
protected:
	virtual Virtualtrees::Basetree::TTreeOptionsClass __fastcall GetOptionsClass();
	
public:
	__property Canvas;
	__property LastDragEffect;
	__property CheckImageKind = {default=1};
	
__published:
	__property Action;
	__property Align = {default=0};
	__property Alignment = {default=0};
	__property Anchors = {default=3};
	__property AnimationDuration = {default=200};
	__property AutoExpandDelay = {default=1000};
	__property AutoScrollDelay = {default=1000};
	__property AutoScrollInterval = {default=1};
	__property Background;
	__property BackgroundOffsetX;
	__property BackgroundOffsetY;
	__property BiDiMode;
	__property BevelEdges = {default=15};
	__property BevelInner = {index=0, default=2};
	__property BevelOuter = {index=1, default=1};
	__property BevelKind = {default=0};
	__property BevelWidth = {default=1};
	__property BorderStyle = {default=1};
	__property BottomSpace;
	__property ButtonFillMode = {default=0};
	__property ButtonStyle = {default=0};
	__property BorderWidth = {default=0};
	__property ChangeDelay = {default=0};
	__property ClipboardFormats;
	__property Color = {default=-16777211};
	__property Colors;
	__property Constraints;
	__property Ctl3D;
	__property CustomCheckImages;
	__property DefaultNodeHeight;
	__property DefaultPasteMode = {default=4};
	__property DragCursor = {default=-12};
	__property DragHeight = {default=350};
	__property DragKind = {default=0};
	__property DragImageKind = {default=0};
	__property DragMode = {default=0};
	__property DragOperations = {default=3};
	__property DragType = {default=0};
	__property DragWidth = {default=200};
	__property DrawSelectionMode = {default=0};
	__property EditDelay = {default=1000};
	__property Enabled = {default=1};
	__property Font;
	__property Header;
	__property HintMode = {default=0};
	__property HotCursor = {default=0};
	__property Images;
	__property IncrementalSearch = {default=1};
	__property IncrementalSearchDirection = {default=0};
	__property IncrementalSearchStart = {default=2};
	__property IncrementalSearchTimeout = {default=1000};
	__property Indent;
	__property LineMode = {default=0};
	__property LineStyle = {default=1};
	__property Margin;
	__property NodeAlignment = {default=2};
	__property NodeDataSize = {default=-1};
	__property OperationCanceled;
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property RootNodeCount = {default=0};
	__property ScrollBarOptions;
	__property SelectionBlendFactor = {default=128};
	__property SelectionCurveRadius = {default=0};
	__property ShowHint;
	__property StateImages;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TextMargin;
	__property Virtualtrees::Basetree::TVirtualTreeOptions* TreeOptions = {read=GetOptions, write=SetOptions};
	__property Visible = {default=1};
	__property WantTabs = {default=0};
	__property OnAddToSelection;
	__property OnAdvancedHeaderDraw;
	__property OnAfterAutoFitColumn;
	__property OnAfterAutoFitColumns;
	__property OnAfterCellPaint;
	__property OnAfterColumnExport;
	__property OnAfterColumnWidthTracking;
	__property OnAfterGetMaxColumnWidth;
	__property OnAfterHeaderExport;
	__property OnAfterHeaderHeightTracking;
	__property OnAfterItemErase;
	__property OnAfterItemPaint;
	__property OnAfterNodeExport;
	__property OnAfterPaint;
	__property OnAfterTreeExport;
	__property OnBeforeAutoFitColumn;
	__property OnBeforeAutoFitColumns;
	__property OnBeforeCellPaint;
	__property OnBeforeColumnExport;
	__property OnBeforeColumnWidthTracking;
	__property OnBeforeDrawTreeLine;
	__property OnBeforeGetMaxColumnWidth;
	__property OnBeforeHeaderExport;
	__property OnBeforeHeaderHeightTracking;
	__property OnBeforeItemErase;
	__property OnBeforeItemPaint;
	__property OnBeforeNodeExport;
	__property OnBeforePaint;
	__property OnBeforeTreeExport;
	__property OnCanSplitterResizeColumn;
	__property OnCanSplitterResizeHeader;
	__property OnCanSplitterResizeNode;
	__property OnChange;
	__property OnChecked;
	__property OnChecking;
	__property OnClick;
	__property OnCollapsed;
	__property OnCollapsing;
	__property OnColumnChecked;
	__property OnColumnChecking;
	__property OnColumnClick;
	__property OnColumnDblClick;
	__property OnColumnExport;
	__property OnColumnResize;
	__property OnColumnVisibilityChanged;
	__property OnColumnWidthDblClickResize;
	__property OnColumnWidthTracking;
	__property OnCompareNodes;
	__property OnContextPopup;
	__property OnCreateDataObject;
	__property OnCreateDragManager;
	__property OnCreateEditor;
	__property OnDblClick;
	__property OnDragAllowed;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDrawHint;
	__property OnDrawNode;
	__property OnEdited;
	__property OnEditing;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEndOperation;
	__property OnEnter;
	__property OnExit;
	__property OnExpanded;
	__property OnExpanding;
	__property OnFocusChanged;
	__property OnFocusChanging;
	__property OnFreeNode;
	__property OnGetCellIsEmpty;
	__property OnGetCursor;
	__property OnGetHeaderCursor;
	__property OnGetHelpContext;
	__property OnGetHintKind;
	__property OnGetHintSize;
	__property OnGetImageIndex;
	__property OnGetImageIndexEx;
	__property OnGetLineStyle;
	__property OnGetNodeDataSize;
	__property OnGetNodeWidth;
	__property OnGetPopupMenu;
	__property OnGetUserClipboardFormats;
	__property OnHeaderAddPopupItem;
	__property OnHeaderClick;
	__property OnHeaderDblClick;
	__property OnHeaderDragged;
	__property OnHeaderDraggedOut;
	__property OnHeaderDragging;
	__property OnHeaderDraw;
	__property OnHeaderDrawQueryElements;
	__property OnHeaderHeightTracking;
	__property OnHeaderHeightDblClickResize;
	__property OnHeaderMouseDown;
	__property OnHeaderMouseMove;
	__property OnHeaderMouseUp;
	__property OnHotChange;
	__property OnIncrementalSearch;
	__property OnInitChildren;
	__property OnInitNode;
	__property OnKeyAction;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnLoadNode;
	__property OnLoadTree;
	__property OnMeasureItem;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnNodeClick;
	__property OnNodeCopied;
	__property OnNodeCopying;
	__property OnNodeDblClick;
	__property OnNodeExport;
	__property OnNodeHeightTracking;
	__property OnNodeHeightDblClickResize;
	__property OnNodeMoved;
	__property OnNodeMoving;
	__property OnPaintBackground;
	__property OnPrepareButtonBitmaps;
	__property OnRemoveFromSelection;
	__property OnRenderOLEData;
	__property OnResetNode;
	__property OnResize;
	__property OnSaveNode;
	__property OnSaveTree;
	__property OnScroll;
	__property OnShowScrollBar;
	__property OnStartDock;
	__property OnStartDrag;
	__property OnStartOperation;
	__property OnStateChange;
	__property OnStructureChange;
	__property OnUpdating;
	__property OnCanResize;
	__property OnGesture;
	__property Touch;
	__property StyleElements = {default=7};
public:
	/* TBaseVirtualTree.Create */ inline __fastcall virtual TVirtualDrawTree(System::Classes::TComponent* AOwner) : TCustomVirtualDrawTree(AOwner) { }
	/* TBaseVirtualTree.Destroy */ inline __fastcall virtual ~TVirtualDrawTree() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualDrawTree(HWND ParentWindow) : TCustomVirtualDrawTree(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Drawtree */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_DRAWTREE)
using namespace Virtualtrees::Drawtree;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_DrawtreeHPP
