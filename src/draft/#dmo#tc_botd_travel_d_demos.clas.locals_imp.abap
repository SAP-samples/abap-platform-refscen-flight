*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class ltd_fields_handler definition final create public.
  public section.
    interfaces if_botd_bufdbl_fields_handler.

    data max_travel_uuid type i value 0.
    data max_booking_uuid type i value 0.

endclass.

class ltd_fields_handler implementation.

  method if_botd_bufdbl_fields_handler~set_readonly_fields.
    case entity_name.
      when '/DMO/R_TRAVEL_D'.
        case operation.
          when if_abap_behv=>op-m-create.
            data create_instances type table for create /DMO/R_TRAVEL_D.
            create_instances = instances.
            loop at create_instances assigning field-symbol(<instance>).
              <instance>-TravelUUID = max_travel_uuid + 1.
              max_travel_uuid += 1.
            endloop.
            instances = create_instances.

          when if_abap_behv=>op-m-create_ba.
            case association_name.
              when '_BOOKING'.
                data cba_instances type table for create /DMO/R_TRAVEL_D\_Booking.
                cba_instances = instances.
                loop at cba_instances assigning field-symbol(<cba_instance>).
                  loop at <cba_instance>-%target assigning field-symbol(<target_instance>).
                    <target_instance>-BookingUUID = max_booking_uuid + 1.
                    max_booking_uuid += 1.
                  endloop.
                endloop.
                instances = cba_instances.
            endcase.
        endcase.
    endcase.
  endmethod.

endclass.
