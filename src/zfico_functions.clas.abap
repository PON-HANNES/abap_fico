class ZFICO_FUNCTIONS definition
  public
  final
  create public .

public section.

  class-methods IS_SUB_FUNCTION_ACTIVE
    importing
      value(IV_BUKRS) type BUKRS
      value(IV_TABNAME) type TABNAME
    returning
      value(RV_VALUE) type CHAR1 .
protected section.
private section.
ENDCLASS.



CLASS ZFICO_FUNCTIONS IMPLEMENTATION.


  METHOD IS_SUB_FUNCTION_ACTIVE.
    SELECT SINGLE sub_option FROM (iv_tabname) WHERE bukrs = @iv_bukrs INTO @rv_value.
  ENDMETHOD.
ENDCLASS.
