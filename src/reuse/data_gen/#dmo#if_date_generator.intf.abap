INTERFACE /dmo/if_date_generator
  PUBLIC.
  CLASS-METHODS get_instance
    RETURNING VALUE(date_generator) TYPE REF TO /dmo/cl_date_generator.

  CLASS-METHODS date_to_timestamp
    IMPORTING !date            TYPE d
    RETURNING VALUE(timestamp) TYPE timestampl.

  METHODS generate_random_time
    RETURNING VALUE(result) TYPE i.

  METHODS generate_date
    RETURNING VALUE(result) TYPE d.

  METHODS generate_new_date_with_offset
    IMPORTING reference_date TYPE d
    RETURNING VALUE(result)  TYPE d.

  METHODS get_date_today
    RETURNING VALUE(result) TYPE d.

  METHODS generate_date_in_range
    IMPORTING from_date     TYPE d
              to_date       TYPE d
    RETURNING VALUE(result) TYPE d.

  METHODS calc_days_before_ref_or_today
    IMPORTING reference_date                    TYPE d OPTIONAL
    RETURNING VALUE(result) TYPE d
    RAISING   cx_parameter_invalid_range
              cx_parameter_invalid_type.

ENDINTERFACE.
