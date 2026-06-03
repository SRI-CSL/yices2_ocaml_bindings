/* Diagnostic instrumentation for the intermittent
   "Failed to reset signal stack (err 22)" crash at process exit. */

#define _GNU_SOURCE

#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

#ifdef __APPLE__
#include <dlfcn.h>
#endif

#include <caml/mlvalues.h>

#define NSIG_MAX 32

static const int g_probe_signals[] = {
  SIGSEGV, SIGBUS, SIGFPE, SIGILL, SIGTRAP, SIGSYS
};
#define N_PROBE_SIGNALS ((int)(sizeof(g_probe_signals) / sizeof(g_probe_signals[0])))

static struct sigaction g_old[NSIG_MAX];
static volatile sig_atomic_t g_count[NSIG_MAX];

static void forward(const struct sigaction *old, int sig, siginfo_t *info, void *ctx)
{
  if (old->sa_flags & SA_SIGINFO) {
    if (old->sa_sigaction) {
      old->sa_sigaction(sig, info, ctx);
      return;
    }
  } else if (old->sa_handler != SIG_DFL && old->sa_handler != SIG_IGN
             && old->sa_handler != NULL) {
    old->sa_handler(sig);
    return;
  }
  signal(sig, SIG_DFL);
  raise(sig);
}

static void probe_handler(int sig, siginfo_t *info, void *ctx)
{
  if (sig >= 0 && sig < NSIG_MAX) g_count[sig]++;
  forward(&g_old[sig], sig, info, ctx);
}

static int total_faults(void)
{
  int i;
  int t = 0;
  for (i = 0; i < NSIG_MAX; i++) t += (int)g_count[i];
  return t;
}

#ifdef __APPLE__
/* macOS-only DYLD interposers. They are diagnostic only: they log libraries
   that change fault signal handlers or the alternate signal stack. */
static int is_fault_sig(int s)
{
  return s == SIGSEGV || s == SIGBUS || s == SIGFPE
      || s == SIGILL  || s == SIGTRAP || s == SIGSYS;
}

static const char *owner_of(void *h)
{
  Dl_info info;
  if (h == (void*)probe_handler) return "MINE";
  if (h == (void*)SIG_DFL) return "DFL";
  if (h == (void*)SIG_IGN) return "IGN";
  if (dladdr(h, &info) && info.dli_sname) return info.dli_sname;
  return "other";
}

static void log_caller(const char *what, int sig, int onstack, void *h)
{
  void *caller = __builtin_return_address(1);
  Dl_info info;
  const char *fn = "?";
  const char *sym = "?";
  if (dladdr(caller, &info)) {
    if (info.dli_fname) fn = info.dli_fname;
    if (info.dli_sname) sym = info.dli_sname;
  }
  fprintf(stderr, "[interpose] %s(sig=%d onstack=%d handler=%s) caller=%s @ %s\n",
          what, sig, onstack, h ? owner_of(h) : "n/a", sym, fn);
  fflush(stderr);
}

static int (*real_sigaction)(int, const struct sigaction*, struct sigaction*) = NULL;
static int my_sigaction(int sig, const struct sigaction *act, struct sigaction *old)
{
  if (!real_sigaction) real_sigaction = dlsym(RTLD_NEXT, "sigaction");
  if (act && is_fault_sig(sig)) {
    void *h = (act->sa_flags & SA_SIGINFO) ? (void*)act->sa_sigaction
                                           : (void*)act->sa_handler;
    log_caller("sigaction", sig, (act->sa_flags & SA_ONSTACK) ? 1 : 0, h);
  }
  return real_sigaction ? real_sigaction(sig, act, old) : -1;
}

typedef void (*sighandler_t)(int);
static sighandler_t (*real_signal)(int, sighandler_t) = NULL;
static sighandler_t my_signal(int sig, sighandler_t h)
{
  if (!real_signal) real_signal = dlsym(RTLD_NEXT, "signal");
  if (is_fault_sig(sig)) log_caller("signal", sig, -1, (void*)h);
  return real_signal(sig, h);
}

static int (*real_sigaltstack)(const stack_t*, stack_t*) = NULL;
static int my_sigaltstack(const stack_t *ss, stack_t *old)
{
  if (!real_sigaltstack) real_sigaltstack = dlsym(RTLD_NEXT, "sigaltstack");
  if (ss) log_caller("sigaltstack-SET", -1,
                     (ss->ss_flags & SS_DISABLE) ? 0 : 1, NULL);
  return real_sigaltstack ? real_sigaltstack(ss, old) : -1;
}

__attribute__((used)) static struct { const void *a; const void *b; }
  _ip_sigaction __attribute__((section("__DATA,__interpose"))) =
  { (const void*)my_sigaction, (const void*)sigaction };
__attribute__((used)) static struct { const void *a; const void *b; }
  _ip_signal __attribute__((section("__DATA,__interpose"))) =
  { (const void*)my_signal, (const void*)signal };
__attribute__((used)) static struct { const void *a; const void *b; }
  _ip_sigaltstack __attribute__((section("__DATA,__interpose"))) =
  { (const void*)my_sigaltstack, (const void*)sigaltstack };
#endif

static volatile sig_atomic_t g_clear_ran = 0;

static void clear_handler(int sig, siginfo_t *info, void *ctx)
{
  (void)sig;
  (void)info;
  (void)ctx;
  g_clear_ran = 1;
}

CAMLprim value caml_sigalt_clear(value unit)
{
  struct sigaction sa, old;
  stack_t ss;
  (void)unit;
  memset(&sa, 0, sizeof(sa));
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_ONSTACK | SA_SIGINFO;
  sa.sa_sigaction = clear_handler;
  g_clear_ran = 0;
  if (sigaction(SIGUSR2, &sa, &old) == 0) {
    raise(SIGUSR2);
    sigaction(SIGUSR2, &old, NULL);
  }
  if (sigaltstack(NULL, &ss) != 0) return Val_int(-1);
  return Val_int((ss.ss_flags & SS_ONSTACK) ? 1 : 0);
}

CAMLprim value caml_sigalt_install_probe(value unit)
{
  struct sigaction sa;
  int i;
  memset(&sa, 0, sizeof(sa));
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sa.sa_sigaction = probe_handler;
  for (i = 0; i < N_PROBE_SIGNALS; i++) {
    int s = g_probe_signals[i];
    sigaction(s, &sa, &g_old[s]);
  }
  return unit;
}

CAMLprim value caml_sigalt_segv_count(value unit)
{
  (void)unit;
  return Val_int(total_faults());
}

CAMLprim value caml_sigalt_raw_exit(value code)
{
  fflush(NULL);
  _exit(Int_val(code));
  return Val_unit;
}

CAMLprim value caml_sigalt_scan_onstack(value label)
{
  const char *l = String_val(label);
  int sig;
  fprintf(stderr, "[sigalt-scan:%s] signals with SA_ONSTACK:", l);
  for (sig = 1; sig < NSIG_MAX; sig++) {
    struct sigaction cur;
    if (sigaction(sig, NULL, &cur) == 0 && (cur.sa_flags & SA_ONSTACK)) {
      fprintf(stderr, " %d", sig);
    }
  }
  fprintf(stderr, "\n");

  {
    static const int key[] = { SIGFPE, SIGBUS, SIGSEGV };
    int k;
    fprintf(stderr, "[sigalt-owner:%s]", l);
    for (k = 0; k < 3; k++) {
      struct sigaction cur;
      int s = key[k];
      void *h;
      const char *tag;
      if (sigaction(s, NULL, &cur) != 0) {
        fprintf(stderr, " sig%d=err", s);
        continue;
      }
      h = (cur.sa_flags & SA_SIGINFO) ? (void*)cur.sa_sigaction
                                      : (void*)cur.sa_handler;
      if (h == (void*)probe_handler) tag = "MINE";
      else if (h == (void*)SIG_DFL) tag = "DFL";
      else if (h == (void*)SIG_IGN) tag = "IGN";
      else tag = "other";
      fprintf(stderr, " sig%d=%s%s", s, tag,
              (cur.sa_flags & SA_ONSTACK) ? "(onstk)" : "");
    }
    fprintf(stderr, "\n");
  }
  fflush(stderr);
  return Val_unit;
}

CAMLprim value caml_sigalt_wait(value secs)
{
  fprintf(stderr, "[wait] pid=%d sleeping %d s for debugger attach...\n",
          (int)getpid(), Int_val(secs));
  fflush(stderr);
  sleep((unsigned)Int_val(secs));
  fprintf(stderr, "[wait] resuming\n");
  fflush(stderr);
  return Val_unit;
}

CAMLprim value caml_sigalt_onstack(value unit)
{
  stack_t ss;
  char probe;
  void *sp = (void*)&probe;
  (void)unit;
  if (sigaltstack(NULL, &ss) != 0) return Val_int(-1);
  if (ss.ss_flags & SS_ONSTACK) {
    char *base = (char*)ss.ss_sp;
    int in_alt = (sp >= (void*)base && sp < (void*)(base + ss.ss_size));
    fprintf(stderr, "[onstack-detect] sp=%p alt=[%p,%p) sp_in_altstack=%d\n",
            sp, (void*)base, (void*)(base + ss.ss_size), in_alt);
    fflush(stderr);
    return Val_int(1);
  }
  return Val_int(0);
}

CAMLprim value caml_sigalt_query(value label)
{
  stack_t ss;
  const char *l = String_val(label);
  if (sigaltstack(NULL, &ss) != 0) {
    fprintf(stderr, "[sigalt:%s] sigaltstack query failed errno=%d\n", l, errno);
  } else {
    int i;
    char probe;
    void *sp = (void*)&probe;
    char *base = (char*)ss.ss_sp;
    int in_alt = (sp >= (void*)base && sp < (void*)(base + ss.ss_size));
    fprintf(stderr,
            "[sigalt:%s] ss_sp=%p ss_size=%lu ss_flags=0x%x onstack=%d disable=%d faults=%d",
            l, ss.ss_sp, (unsigned long)ss.ss_size, (unsigned)ss.ss_flags,
            (ss.ss_flags & SS_ONSTACK) ? 1 : 0,
            (ss.ss_flags & SS_DISABLE) ? 1 : 0,
            total_faults());
    for (i = 0; i < NSIG_MAX; i++) {
      if (g_count[i]) fprintf(stderr, " sig%d=%d", i, (int)g_count[i]);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "[sigalt-sp:%s] sp=%p alt=[%p,%p) sp_in_altstack=%d\n",
            l, sp, (void*)base, (void*)(base + ss.ss_size), in_alt);
  }
  fflush(stderr);
  return Val_unit;
}
