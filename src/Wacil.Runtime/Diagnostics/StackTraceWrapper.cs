namespace Wacil.Runtime.Diagnostics;

using System;
using System.Diagnostics;
using System.Reflection;
using System.Text;

/// <summary>Provides a wrapper over a <see cref="StackTrace"/>.</summary>
public class StackTraceWrapper {
    /// <summary>Gets the underlying stack trace.</summary>
    public StackTrace Original { get; }

    /// <summary>Initializes a <see cref="StackTraceWrapper"/> wrapper over the specified <see cref="StackTrace"/> instance.</summary>
    /// <exception cref="ArgumentNullException">
    /// Thrown if the <paramref name="original"/> stack trace is <see langword="null"/>.
    /// </exception>
    public StackTraceWrapper(StackTrace original) {
        Original = original ?? throw new ArgumentNullException(nameof(original));
    }

    /// <summary>Writes the contents of this stack trace to a <see cref="StringBuilder"/>.</summary>
    public void ToString(StringBuilder writer) {
        ArgumentNullException.ThrowIfNull(writer);

        foreach(StackFrame frame in Original.GetFrames()) {
            MethodBase? method = frame.GetMethod();
            if (method != null) {
                writer.AppendLine();
                writer.Append("   at ");

                Type? ty = method.DeclaringType;
                if (ty != null) {
                    string fullTypeName = ty.FullName!;
                    writer.EnsureCapacity(writer.Capacity + fullTypeName.Length + 1);

                    foreach(char c in fullTypeName) {
                        writer.Append(c == '.' ? '+' : c);
                    }

                    writer.Append('.');
                }

                CustomNameAttribute? customNameAttribute = method.GetCustomAttribute<CustomNameAttribute>();
                writer.Append(String.IsNullOrEmpty(customNameAttribute?.Name) ? method.Name : customNameAttribute!.Name);

                // TODO: Get generic arguments as well

                ParameterInfo[]? methodParameterInfos;
                try {
                    methodParameterInfos = method.GetParameters();
                } catch {
                    methodParameterInfos = null;
                }

                if (methodParameterInfos != null) {
                    writer.Append('(');
                    bool isFirstParameter = true;
                    foreach(ParameterInfo parameter in methodParameterInfos) {
                        if (isFirstParameter) {
                            isFirstParameter = false;
                        } else {
                            writer.Append(", ");
                        }

                        writer.Append(parameter.ParameterType.Name);
                        string? name = parameter.Name;
                        if (!String.IsNullOrEmpty(name)) {
                            writer.Append(' ').Append(name);
                        }
                    }
                    writer.Append(')');
                }

                int ilMethodOffset = frame.GetILOffset();
                if (ilMethodOffset != -1) {
                    string? methodFileName = frame.GetFileName();

                    if (methodFileName != null) {
                        writer.Append(" in ").Append(methodFileName).Append(':').Append(frame.GetFileLineNumber()).Append(':')
                            .Append(frame.GetFileColumnNumber());
                    } else {
                        writer.AppendFormat("at IL offset 0x{0:X4}", ilMethodOffset);
                    }
                }
            }
        }
    }
}
