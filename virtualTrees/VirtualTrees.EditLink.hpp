// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.EditLink.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_EditlinkHPP
#define Virtualtrees_EditlinkHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Messages.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <VirtualTrees.hpp>
#include <VirtualTrees.Types.hpp>
#include <VirtualTrees.BaseTree.hpp>
#include <Vcl.Forms.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Editlink
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTEdit;
class DELPHICLASS TBaseEditLink;
class DELPHICLASS TWinControlEditLink;
class DELPHICLASS TStringEditLink;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVTEdit : public Vcl::Stdctrls::TCustomEdit
{
	typedef Vcl::Stdctrls::TCustomEdit inherited;
	
private:
	MESSAGE void __fastcall CMAutoAdjust(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMRelease(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMChar &Message);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMDestroy &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMGetDlgCode &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKeyDown &Message);
	
protected:
	Virtualtrees::Basetree::_di_IVTEditLink FRefLink;
	TStringEditLink* FLink;
	virtual void __fastcall AutoAdjustSize();
	virtual int __fastcall CalcMinHeight();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual System::Types::TSize __fastcall GetTextSize();
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	
public:
	__fastcall TVTEdit(TStringEditLink* Link);
	void __fastcall ClearLink();
	void __fastcall ClearRefLink();
	virtual void __fastcall Release();
	__property AutoSelect = {default=1};
	__property AutoSize = {default=1};
	__property BorderStyle = {default=1};
	__property CharCase = {default=0};
	__property HideSelection = {default=1};
	__property MaxLength = {default=0};
	__property OEMConvert = {default=0};
	__property PasswordChar = {default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TVTEdit(HWND ParentWindow) : Vcl::Stdctrls::TCustomEdit(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TVTEdit() { }
	
};


typedef void __fastcall (__closure *TEditLinkEditEvent)(TBaseEditLink* Sender, bool &Result);

typedef void __fastcall (__closure *TEditLinkPrepareEditEvent)(TBaseEditLink* Sender, Vcl::Controls::TControl* &Edit, bool &Result);

class PASCALIMPLEMENTATION TBaseEditLink : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	Vcl::Controls::TControl* FEdit;
	Virtualtrees::TCustomVirtualStringTree* FTree;
	Virtualtrees::Types::PVirtualNode FNode;
	Virtualtrees::Basetree::TColumnIndex FColumn;
	bool FStopping;
	System::Classes::TAlignment FAlignment;
	System::Classes::TBiDiMode FBiDiMode;
	TEditLinkPrepareEditEvent FOnPrepareEdit;
	TEditLinkEditEvent FOnBeginEdit;
	TEditLinkEditEvent FOnEndEdit;
	TEditLinkEditEvent FOnCancelEdit;
	void __fastcall SetEdit(Vcl::Controls::TControl* const Value);
	
public:
	virtual bool __stdcall BeginEdit();
	virtual bool __stdcall CancelEdit();
	virtual bool __stdcall EndEdit();
	virtual System::Types::TRect __stdcall GetBounds() = 0 ;
	virtual bool __stdcall PrepareEdit(Virtualtrees::Basetree::TBaseVirtualTree* Tree, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Basetree::TColumnIndex Column);
	virtual void __stdcall ProcessMessage(Winapi::Messages::TMessage &Message) = 0 ;
	virtual void __stdcall SetBounds(System::Types::TRect R) = 0 ;
	virtual void __fastcall DoBeginEdit(bool &Result);
	virtual void __fastcall DoCancelEdit(bool &Result);
	virtual void __fastcall DoEndEdit(bool &Result);
	virtual void __fastcall DoPrepareEdit(bool &Result);
	__property System::Classes::TAlignment Alignment = {read=FAlignment, nodefault};
	__property System::Classes::TBiDiMode BiDiMode = {read=FBiDiMode, nodefault};
	__property Virtualtrees::Basetree::TColumnIndex Column = {read=FColumn, nodefault};
	__property Virtualtrees::Types::PVirtualNode Node = {read=FNode};
	__property Virtualtrees::TCustomVirtualStringTree* Tree = {read=FTree};
	__property bool Stopping = {read=FStopping, nodefault};
	__property TEditLinkEditEvent OnBeginEdit = {read=FOnBeginEdit, write=FOnBeginEdit};
	__property TEditLinkEditEvent OnCancelEdit = {read=FOnCancelEdit, write=FOnCancelEdit};
	__property TEditLinkEditEvent OnEndEdit = {read=FOnEndEdit, write=FOnEndEdit};
	__property TEditLinkPrepareEditEvent OnPrepareEdit = {read=FOnPrepareEdit, write=FOnPrepareEdit};
public:
	/* TObject.Create */ inline __fastcall TBaseEditLink() : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TBaseEditLink() { }
	
private:
	void *__IVTEditLink;	// Virtualtrees::Basetree::IVTEditLink 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}
	operator Virtualtrees::Basetree::_di_IVTEditLink()
	{
		Virtualtrees::Basetree::_di_IVTEditLink intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Virtualtrees::Basetree::IVTEditLink*(void) { return (Virtualtrees::Basetree::IVTEditLink*)&__IVTEditLink; }
	#endif
	
};


class PASCALIMPLEMENTATION TWinControlEditLink : public TBaseEditLink
{
	typedef TBaseEditLink inherited;
	
protected:
	Vcl::Controls::TWinControl* __fastcall GetEdit();
	HIDESBASE void __fastcall SetEdit(Vcl::Controls::TWinControl* const Value);
	
public:
	__fastcall virtual ~TWinControlEditLink();
	virtual bool __stdcall BeginEdit();
	virtual bool __stdcall CancelEdit();
	virtual bool __stdcall EndEdit();
	virtual System::Types::TRect __stdcall GetBounds();
	virtual void __stdcall ProcessMessage(Winapi::Messages::TMessage &Message);
	__property Vcl::Controls::TWinControl* Edit = {read=GetEdit, write=SetEdit};
public:
	/* TObject.Create */ inline __fastcall TWinControlEditLink() : TBaseEditLink() { }
	
};


class PASCALIMPLEMENTATION TStringEditLink : public TWinControlEditLink
{
	typedef TWinControlEditLink inherited;
	
protected:
	System::Types::TRect FTextBounds;
	HIDESBASE TVTEdit* __fastcall GetEdit();
	HIDESBASE void __fastcall SetEdit(TVTEdit* const Value);
	virtual void __fastcall InitializeSelection();
	
public:
	__fastcall TStringEditLink();
	virtual bool __stdcall BeginEdit();
	virtual bool __stdcall CancelEdit();
	virtual bool __stdcall EndEdit();
	virtual bool __stdcall PrepareEdit(Virtualtrees::Basetree::TBaseVirtualTree* Tree, Virtualtrees::Types::PVirtualNode Node, Virtualtrees::Basetree::TColumnIndex Column);
	virtual void __stdcall SetBounds(System::Types::TRect R);
	__property TVTEdit* Edit = {read=GetEdit, write=SetEdit};
public:
	/* TWinControlEditLink.Destroy */ inline __fastcall virtual ~TStringEditLink() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Editlink */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_EDITLINK)
using namespace Virtualtrees::Editlink;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_EditlinkHPP
