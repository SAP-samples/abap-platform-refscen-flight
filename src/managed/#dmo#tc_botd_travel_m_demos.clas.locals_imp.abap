*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class ltd_fields_handler definition final create public.
  public section.
    interfaces if_botd_bufdbl_fields_handler.

    data max_travel_id type i value 0.
    data max_booking_id type i value 0.

endclass.

class ltd_fields_handler implementation.

  method if_botd_bufdbl_fields_handler~set_readonly_fields.
    case entity_name.
      when '/DMO/I_TRAVEL_M'.
        case operation.
          when if_abap_behv=>op-m-create.
            data create_instances type table for create /DMO/I_TRAVEL_M.
            create_instances = instances.
            loop at create_instances assigning field-symbol(<instance>).
              <instance>-Travel_ID = max_travel_id + 1.
              max_travel_id += 1.
            endloop.
            instances = create_instances.

          when if_abap_behv=>op-m-create_ba.
            case association_name.
              when '_BOOKING'.
                data cba_instances type table for create /DMO/I_TRAVEL_M\_Booking.
                cba_instances = instances.
                loop at cba_instances assigning field-symbol(<cba_instance>).
                  loop at <cba_instance>-%target assigning field-symbol(<target_instance>).
                    <target_instance>-Booking_ID = max_booking_id + 1.
                    max_booking_id += 1.
                  endloop.
                endloop.
                instances = cba_instances.
            endcase.
        endcase.
    endcase.
  endmethod.

endclass.
