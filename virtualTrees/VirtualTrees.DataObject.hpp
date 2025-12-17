// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.DataObject.pas' rev: 36.00 (Windows)

#ifndef Virtualtrees_DataobjectHPP
#define Virtualtrees_DataobjectHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <VirtualTrees.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Dataobject
{
//-- forward type declarations -----------------------------------------------
struct TInternalStgMedium;
class DELPHICLASS TVTDataObject;
//-- type declarations -------------------------------------------------------
typedef _di_IDataObject IDataObject;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TInternalStgMedium
{
public:
	System::Word Format;
	Winapi::Activex::TStgMedium Medium;
};
#pragma pack(pop)


typedef System::DynamicArray<TInternalStgMedium> TInternalStgMediumArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDataObject : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	Vcl::Controls::TCustomControl* FOwner;
	System::Classes::TPersistent* FHeader;
	bool FForClipboard;
	Virtualtrees::Types::TFormatEtcArray FFormatEtcArray;
	TInternalStgMediumArray FInternalStgMediumArray;
	_di_IDataAdviseHolder FAdviseHolder;
	
protected:
	System::_di_IInterface __fastcall CanonicalIUnknown(const System::_di_IInterface TestUnknown);
	bool __fastcall EqualFormatEtc(const Winapi::Activex::TFormatEtc &FormatEtc1, const Winapi::Activex::TFormatEtc &FormatEtc2);
	int __fastcall FindFormatEtc(const Winapi::Activex::TFormatEtc &TestFormatEtc, const Virtualtrees::Types::TFormatEtcArray FormatEtcArray);
	Winapi::Activex::PStgMedium __fastcall FindInternalStgMedium(System::Word Format);
	Winapi::Windows::THandle __fastcall HGlobalClone(Winapi::Windows::THandle HGlobal);
	bool __fastcall RenderInternalOLEData(const Winapi::Activex::TFormatEtc &FormatEtcIn, Winapi::Activex::TStgMedium &Medium, HRESULT &OLEResult);
	HRESULT __fastcall StgMediumIncRef(const Winapi::Activex::TStgMedium &InStgMedium, Winapi::Activex::TStgMedium &OutStgMedium, bool CopyInMedium, const IDataObject DataObject);
	__property bool ForClipboard = {read=FForClipboard, nodefault};
	__property Virtualtrees::Types::TFormatEtcArray FormatEtcArray = {read=FFormatEtcArray, write=FFormatEtcArray};
	__property TInternalStgMediumArray InternalStgMediumArray = {read=FInternalStgMediumArray, write=FInternalStgMediumArray};
	__property Vcl::Controls::TCustomControl* Owner = {read=FOwner};
	
public:
	__fastcall TVTDataObject(Vcl::Controls::TCustomControl* AOwner, bool ForClipboard)/* overload */;
	__fastcall TVTDataObject(System::Classes::TPersistent* AHeader, Vcl::Controls::TCustomControl* AOwner)/* overload */;
	__fastcall virtual ~TVTDataObject();
	virtual HRESULT __stdcall DAdvise(const Winapi::Activex::TFormatEtc &FormatEtc, int advf, const _di_IAdviseSink advSink, /* out */ int &dwConnection);
	virtual HRESULT __stdcall DUnadvise(int dwConnection);
	virtual HRESULT __stdcall EnumDAdvise(/* out */ _di_IEnumSTATDATA &enumAdvise);
	virtual HRESULT __stdcall EnumFormatEtc(int Direction, /* out */ _di_IEnumFORMATETC &EnumFormatEtc);
	virtual HRESULT __stdcall GetCanonicalFormatEtc(const Winapi::Activex::TFormatEtc &FormatEtc, /* out */ Winapi::Activex::TFormatEtc &FormatEtcOut);
	virtual HRESULT __stdcall GetData(const Winapi::Activex::TFormatEtc &FormatEtcIn, /* out */ Winapi::Activex::TStgMedium &Medium);
	virtual HRESULT __stdcall GetDataHere(const Winapi::Activex::TFormatEtc &FormatEtc, /* out */ Winapi::Activex::TStgMedium &Medium);
	virtual HRESULT __stdcall QueryGetData(const Winapi::Activex::TFormatEtc &FormatEtc);
	virtual HRESULT __stdcall SetData(const Winapi::Activex::TFormatEtc &FormatEtc, Winapi::Activex::TStgMedium &Medium, System::LongBool DoRelease);
private:
	void *__IDataObject;	// IDataObject 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0000010E-0000-0000-C000-000000000046}
	operator _di_IDataObject()
	{
		_di_IDataObject intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDataObject*(void) { return (IDataObject*)&__IDataObject; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dataobject */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_DATAOBJECT)
using namespace Virtualtrees::Dataobject;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_DataobjectHPP
