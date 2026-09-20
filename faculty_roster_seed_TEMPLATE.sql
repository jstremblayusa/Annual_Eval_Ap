-- TEMPLATE ONLY: this file contains no real faculty information.
--
-- Make a private copy named faculty_roster_seed.sql. That filename is excluded
-- by .gitignore. Replace the placeholder row with the real roster, run the
-- private copy directly in Supabase SQL Editor, and do not upload it to GitHub.

insert into public.faculty(
  n_number,
  last_name,
  first_name,
  formal_first_name,
  position,
  email,
  system_role,
  display_order
) values
  ('REPLACE_WITH_N_NUMBER', 'LAST NAME', 'PREFERRED FIRST', 'FORMAL FIRST',
   'POSITION', 'faculty.user@example.invalid', 'faculty', 100)
on conflict(n_number) do update set
  last_name = excluded.last_name,
  first_name = excluded.first_name,
  formal_first_name = excluded.formal_first_name,
  position = excluded.position,
  email = excluded.email,
  system_role = excluded.system_role,
  display_order = excluded.display_order,
  active = true;

-- Exactly one private roster row should use system_role = 'admin'.
-- All other rows should use system_role = 'faculty'.
