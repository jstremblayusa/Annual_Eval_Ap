-- Run this once after supabase_setup.sql.
-- It creates links only for faculty who do not already have an active link.
-- Existing links and all evaluation data remain unchanged.

create or replace function public.issue_missing_access_links()
returns table(last_name text, first_name text, email text, access_token text)
language plpgsql security definer set search_path=public,extensions as $$
declare
  f record;
  raw_token text;
begin
  for f in
    select x.faculty_id, x.last_name, x.first_name, x.email
    from public.faculty x
    where x.active
      and not exists (
        select 1 from public.access_links a
        where a.faculty_id=x.faculty_id and a.active and a.revoked_at is null
      )
    order by x.display_order
  loop
    raw_token := encode(gen_random_bytes(32), 'hex');
    insert into public.access_links(faculty_id, token_hash)
    values (f.faculty_id, encode(digest(raw_token, 'sha256'), 'hex'));
    last_name := f.last_name;
    first_name := f.first_name;
    email := f.email;
    access_token := raw_token;
    return next;
  end loop;
end;
$$;

revoke all on function public.issue_missing_access_links() from public, anon, authenticated;

select * from public.issue_missing_access_links();

-- Save the returned tokens immediately. Supabase stores only their hashes.
-- After deployment, append each token to the app URL as:
-- ?access=RETURNED_TOKEN
