// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.DragnDrop.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_DragndropHPP
#define Virtualtrees_DragndropHPP

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
#include <Winapi.ShlObj.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <VirtualTrees.Types.hpp>
#include <VirtualTrees.BaseTree.hpp>
#include <VirtualTrees.Header.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Dragndrop
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TEnumFormatEtc;
class DELPHICLASS TVTDragManager;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TEnumFormatEtc : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	Virtualtrees::Basetree::TFormatEtcArray FFormatEtcArray;
	int FCurrentIndex;
	
public:
	__fastcall TEnumFormatEtc(const Virtualtrees::Basetree::TFormatEtcArray AFormatEtcArray);
	HRESULT __stdcall Clone(/* out */ _di_IEnumFORMATETC &Enum);
	HRESULT __stdcall Next(int celt, /* out */ void *elt, System::PLongInt pceltFetched);
	HRESULT __stdcall Reset();
	HRESULT __stdcall Skip(int celt);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEnumFormatEtc() { }
	
private:
	void *__IEnumFORMATETC;	// IEnumFORMATETC 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000103-0000-0000-C000-000000000046}
	operator _di_IEnumFORMATETC()
	{
		_di_IEnumFORMATETC intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IEnumFORMATETC*(void) { return (IEnumFORMATETC*)&__IEnumFORMATETC; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDragManager : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	Virtualtrees::Basetree::TBaseVirtualTree* FOwner;
	Virtualtrees::Basetree::TBaseVirtualTree* FDragSource;
	Virtualtrees::Header::TVTHeader* FHeader;
	bool FIsDropTarget;
	Virtualtrees::Types::IDataObject FDataObject;
	_di_IDropTargetHelper FDropTargetHelper;
	System::LongBool FFullDragging;
	Virtualtrees::Types::IDataObject __stdcall GetDataObject();
	Virtualtrees::Basetree::TBaseVirtualTree* __stdcall GetDragSource();
	bool __stdcall GetIsDropTarget();
	
public:
	__fastcall virtual TVTDragManager(Virtualtrees::Basetree::TBaseVirtualTree* AOwner);
	__fastcall virtual ~TVTDragManager();
	HRESULT __stdcall DragEnter(const Virtualtrees::Types::IDataObject DataObject, int KeyState, System::Types::TPoint Pt, System::LongInt &Effect);
	HRESULT __stdcall DragLeave();
	HRESULT __stdcall DragOver(int KeyState, System::Types::TPoint Pt, System::LongInt &Effect);
	HRESULT __stdcall Drop(const Virtualtrees::Types::IDataObject DataObject, int KeyState, System::Types::TPoint Pt, int &Effect);
	void __stdcall ForceDragLeave();
	HRESULT __stdcall GiveFeedback(int Effect);
	HRESULT __stdcall QueryContinueDrag(System::LongBool EscapePressed, int KeyState);
	__classmethod Virtualtrees::Basetree::TBaseVirtualTree* __fastcall GetTreeFromDataObject(const Virtualtrees::Types::TVTDragDataObject DataObject);
private:
	void *__IDropTarget;	// IDropTarget 
	void *__IDropSource;	// IDropSource 
	void *__IVTDragManager;	// Virtualtrees::Basetree::IVTDragManager 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000122-0000-0000-C000-000000000046}
	operator _di_IDropTarget()
	{
		_di_IDropTarget intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDropTarget*(void) { return (IDropTarget*)&__IDropTarget; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000121-0000-0000-C000-000000000046}
	operator _di_IDropSource()
	{
		_di_IDropSource intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDropSource*(void) { return (IDropSource*)&__IDropSource; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C4B25559-14DA-446B-8901-0C879000EB16}
	operator Virtualtrees::Basetree::_di_IVTDragManager()
	{
		Virtualtrees::Basetree::_di_IVTDragManager intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Virtualtrees::Basetree::IVTDragManager*(void) { return (Virtualtrees::Basetree::IVTDragManager*)&__IVTDragManager; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Winapi::Activex::TFormatEtc StandardOLEFormat;
}	/* namespace Dragndrop */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_DRAGNDROP)
using namespace Virtualtrees::Dragndrop;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_DragndropHPP
